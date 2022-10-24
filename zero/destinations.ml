open! Core

let fd_destination_from buf file ~do_close =
  let flush () =
    Iobuf.flip_lo buf;
    Iobuf_unix.write buf file;
    Iobuf.reset buf
  in
  let module Dest = struct
    let next_buf ~ensure_capacity =
      flush ();
      if ensure_capacity > Iobuf.length buf
      then failwith "Not enough buffer space in [direct_file_destination]";
      buf
    ;;

    let close () =
      flush ();
      if do_close then Core_unix.close file
    ;;
  end
  in
  (module Dest : Writer_intf.Destination)
;;

let direct_file_destination ?(buffer_size = 4096 * 16) ~filename () =
  let buf = Iobuf.create ~len:buffer_size in
  let file = Core_unix.openfile ~mode:[ O_CREAT; O_TRUNC; O_RDWR ] filename in
  fd_destination_from buf file ~do_close:true
;;

let fd_destination ?(buffer_size = 4096 * 16) ~fd () =
  let buf = Iobuf.create ~len:buffer_size in
  fd_destination_from buf fd ~do_close:false
;;

let file_destination ~filename () = direct_file_destination ~filename ()

let iobuf_destination buf =
  (* We give out an [Iobuf] with a shared underlying [Bigstring] but different pointers
     so that when this is closed the provided buffer keeps its window, and we can test
     the [Buffer_until_initialized] feature to ignore writes after close.

     This also ensures our logic works when the window of [buf] is narrower than the
     limits because [sub_shared] leads to a buffer with equal window and limits. *)
  let provided_buf = Iobuf.sub_shared buf in
  let module Dest = struct
    (* [next_buf] can be called multiple times even without running out of room, for
       example via [Writer.Expert.force_switch_buffers]. But we can just keep giving back
       the same buffer as long as it has room *)
    let next_buf ~ensure_capacity =
      if ensure_capacity > Iobuf.length provided_buf
      then failwith "No more room in [iobuf_destination]";
      provided_buf
    ;;

    let close () =
      Iobuf.flip_lo provided_buf;
      Iobuf.resize ~len:(Iobuf.length provided_buf) buf;
      Iobuf.resize ~len:0 provided_buf
    ;;
  end
  in
  (module Dest : Writer_intf.Destination)
;;

let black_hole_destination ~len ~touch_memory =
  let buf = Iobuf.create ~len in
  if touch_memory then Iobuf.zero buf;
  let module Dest = struct
    let next_buf ~ensure_capacity =
      Iobuf.reset buf;
      if ensure_capacity > Iobuf.length buf
      then failwith "Record too large for [black_hole_destination]";
      buf
    ;;

    let close () = ()
  end
  in
  (module Dest : Writer_intf.Destination)
;;

(* A [Destination] which keeps buffers it gives out in a list and is able to write the
   contents of those buffers to another [Destination]. *)
module Temp_buffer : sig
  type t =
    { copy_to : (module Writer_intf.Destination) -> unit
    ; dest : (module Writer_intf.Destination)
    }

  val create : unit -> t
end = struct
  type t =
    { copy_to : (module Writer_intf.Destination) -> unit
    ; dest : (module Writer_intf.Destination)
    }

  type internal = { mutable buffers : (read_write, Iobuf.seek) Iobuf.t list }

  let create () =
    let t = { buffers = [] } in
    let module Dest = struct
      let next_buf ~ensure_capacity =
        let capacity = Int.max ensure_capacity 1_000 in
        let buf = Iobuf.create ~len:capacity in
        t.buffers <- buf :: t.buffers;
        buf
      ;;

      (* We have nowhere to flush to *)
      let close () = ()
    end
    in
    let dest = (module Dest : Writer_intf.Destination) in
    let copy_to (module D : Writer_intf.Destination) =
      let in_order_buffers = List.rev t.buffers in
      let out_buf = ref (D.next_buf ~ensure_capacity:0) in
      List.iter in_order_buffers ~f:(fun in_buf ->
        Iobuf.flip_lo in_buf;
        (* Core.print_s [%sexp (in_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]; *)
        let in_buf_len = Iobuf.length in_buf in
        if Iobuf.length !out_buf < in_buf_len
        then out_buf := D.next_buf ~ensure_capacity:in_buf_len;
        Iobuf.Blit_fill.blito ~src:in_buf ~dst:!out_buf ());
      t.buffers <- []
    in
    { copy_to; dest }
  ;;
end

module Buffer_until_initialized = struct
  type state =
    | Buffering_to of Temp_buffer.t
    | Needs_transfer of
        { src : Temp_buffer.t
        ; dst : (module Writer_intf.Destination)
        }
    | Set of (module Writer_intf.Destination)

  type t = { mutable state : state }

  let create () =
    let temp_buffer = Temp_buffer.create () in
    { state = Buffering_to temp_buffer }
  ;;

  let set_destination t destination =
    match t.state with
    | Buffering_to temp_buffer ->
      (* We can't immediately do the transfer because the writer is still using the last
         buffer we gave it, so we need to wait for it to ask for a new buffer. *)
      t.state <- Needs_transfer { src = temp_buffer; dst = destination }
    | Needs_transfer _ | Set _ ->
      failwith "Tried to set Buffer_until_initialized which already had destination"
  ;;

  let to_destination t =
    let module Dest = struct
      let next_buf ~ensure_capacity =
        let (module D) =
          match t.state with
          | Needs_transfer { src; dst } ->
            src.copy_to dst;
            t.state <- Set dst;
            dst
          | Buffering_to temp_buffer -> temp_buffer.dest
          | Set d -> d
        in
        D.next_buf ~ensure_capacity
      ;;

      let close () =
        let (module D) =
          match t.state with
          | Needs_transfer { src; dst } ->
            src.copy_to dst;
            dst
          | Buffering_to temp_buffer -> temp_buffer.dest
          | Set d -> d
        in
        D.close ();
        (* Make it so writes after closing will be gracefully ignored. *)
        t.state <- Set (black_hole_destination ~len:1024 ~touch_memory:false)
      ;;
    end
    in
    (module Dest : Writer_intf.Destination)
  ;;
end
