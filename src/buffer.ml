open Core
open Tracing_zero

module Process = struct
  type t = { pid : int } [@@deriving sexp_of, compare, hash, equal]
end

module Thread = struct
  type t =
    { pid : int
    ; tid : int
    }
  [@@deriving sexp_of, compare, hash, equal]
end

module Ticks = struct
  type t =
    { ticks_per_second : int
    ; base_time : Time_ns_unix.Option.t
    }
end

module State = struct
  module References = struct
    (** Keeps track of what dynamic state is referenced in a span of tracing events. *)
    type t =
      { string_idxs : Hash_set.M(Parser.String_index).t
      ; thread_idxs : Hash_set.M(Parser.Thread_index).t
      ; threads : Hash_set.M(Thread).t
      ; processes : Hash_set.M(Process).t
      }

    let create () =
      { string_idxs = Hash_set.create (module Parser.String_index)
      ; thread_idxs = Hash_set.create (module Parser.Thread_index)
      ; threads = Hash_set.create (module Thread)
      ; processes = Hash_set.create (module Process)
      }
    ;;
  end

  (** Dynamic tracing state updated by tracing events. *)
  type t =
    { strings : string Hashtbl.M(Parser.String_index).t (** Interned strings *)
    ; threads : Parser.Thread.t Hashtbl.M(Parser.Thread_index).t
    (** Interned thread IDs *)
    ; thread_names : Parser.String_index.t Hashtbl.M(Thread).t
    ; process_names : Parser.String_index.t Hashtbl.M(Process).t
    ; mutable ticks : Ticks.t option
    }

  let create () =
    { strings = Hashtbl.create (module Parser.String_index)
    ; threads = Hashtbl.create (module Parser.Thread_index)
    ; thread_names = Hashtbl.create (module Thread)
    ; process_names = Hashtbl.create (module Process)
    ; ticks = None
    }
  ;;

  let copy t =
    { strings = Hashtbl.copy t.strings
    ; threads = Hashtbl.copy t.threads
    ; thread_names = Hashtbl.copy t.thread_names
    ; process_names = Hashtbl.copy t.process_names
    ; ticks = t.ticks
    }
  ;;

  let filter t ~(references : References.t) =
    let strings =
      Hashtbl.filter_keys t.strings ~f:(fun k0 ->
        Hash_set.exists references.string_idxs ~f:(fun k1 ->
          Parser.String_index.equal k0 k1))
    in
    let threads =
      Hashtbl.filter_keys t.threads ~f:(fun k0 ->
        Hash_set.exists references.thread_idxs ~f:(fun k1 ->
          Parser.Thread_index.equal k0 k1))
    in
    let thread_names =
      Hashtbl.filter_keys t.thread_names ~f:(fun k0 ->
        Hash_set.exists references.threads ~f:(fun k1 -> Thread.equal k0 k1))
    in
    let process_names =
      Hashtbl.filter_keys t.process_names ~f:(fun k0 ->
        Hash_set.exists references.processes ~f:(fun k1 -> Process.equal k0 k1))
    in
    { strings; threads; thread_names; process_names; ticks = t.ticks }
  ;;

  let process t record =
    let open Parser.Record in
    match record with
    | Interned_string { index; value } -> Hashtbl.set t.strings ~key:index ~data:value
    | Interned_thread { index; value } -> Hashtbl.set t.threads ~key:index ~data:value
    | Process_name_change { name; pid } ->
      Hashtbl.set t.process_names ~key:{ pid } ~data:name
    | Thread_name_change { name; pid; tid } ->
      Hashtbl.set t.thread_names ~key:{ pid; tid } ~data:name
    | Tick_initialization { ticks_per_second; base_time } ->
      t.ticks <- Some Ticks.{ ticks_per_second; base_time }
    | Event _ -> ()
  ;;

  let construct_trace t writer =
    let open Parser.Record in
    let rw = Record_writer.create writer in
    Option.iter t.ticks ~f:(fun { ticks_per_second; base_time } ->
      let record = Tick_initialization { ticks_per_second; base_time } in
      Record_writer.write_record rw ~record);
    Hashtbl.iteri t.strings ~f:(fun ~key ~data ->
      let record = Interned_string { index = key; value = data } in
      Record_writer.write_record rw ~record);
    Hashtbl.iteri t.threads ~f:(fun ~key ~data ->
      let record = Interned_thread { index = key; value = data } in
      Record_writer.write_record rw ~record);
    Hashtbl.iteri t.thread_names ~f:(fun ~key ~data ->
      let record = Thread_name_change { name = data; pid = key.pid; tid = key.tid } in
      Record_writer.write_record rw ~record);
    Hashtbl.iteri t.process_names ~f:(fun ~key ~data ->
      let record = Process_name_change { name = data; pid = key.pid } in
      Record_writer.write_record rw ~record)
  ;;
end

module Checkpoint = struct
  (** Data necessary to describe a span of tracing events. *)
  type t =
    { begin_state : State.t
    ; data : (read_write, Iobuf.seek) Iobuf.t
    ; data_view : (read_write, Iobuf.seek) Iobuf.t
    ; destination : (module Writer_intf.Destination)
    ; refs : State.References.t
    }

  let create ~size_bits ~begin_state =
    let data = Iobuf.create ~len:(Int.pow 2 size_bits) in
    let (module Dest : Writer_intf.Destination) = Destinations.iobuf_destination data in
    { begin_state
    ; destination = (module Dest)
    ; data
    (* [data_view] represents the same window as [data], but will have its [lo] updated
       every time data is written, rather than upon [Dest.close ()].

       This relies on the implementation of [iobuf_destination] always returning the same
       iobuf view of [data]!! *)
    ; data_view = Dest.next_buf ~ensure_capacity:0
    ; refs = State.References.create ()
    }
  ;;

  let process t record =
    let open Parser.Record in
    match record with
    | Interned_string { index; value = _ } -> Hash_set.add t.refs.string_idxs index
    | Interned_thread { index; value = _ } -> Hash_set.add t.refs.thread_idxs index
    | Process_name_change { name; pid } ->
      Hash_set.add t.refs.string_idxs name;
      Hash_set.add t.refs.processes { pid }
    | Thread_name_change { name; pid; tid } ->
      Hash_set.add t.refs.string_idxs name;
      Hash_set.add t.refs.threads { pid; tid }
    | Event { timestamp = _; thread; category; name; arguments; event_type = _ } ->
      Hash_set.add t.refs.string_idxs category;
      Hash_set.add t.refs.string_idxs name;
      Hash_set.add t.refs.thread_idxs thread;
      List.iter arguments ~f:(fun (name, arg) ->
        Hash_set.add t.refs.string_idxs name;
        match arg with
        | Parser.Event_arg.String idx -> Hash_set.add t.refs.string_idxs idx
        | _ -> ())
    | Tick_initialization _ -> ()
  ;;

  let union_refs t0 t1 =
    { State.References.string_idxs =
        Hash_set.union t0.refs.string_idxs t1.refs.string_idxs
    ; thread_idxs = Hash_set.union t0.refs.thread_idxs t1.refs.thread_idxs
    ; threads = Hash_set.union t0.refs.threads t1.refs.threads
    ; processes = Hash_set.union t0.refs.processes t1.refs.processes
    }
  ;;

  let write t writer =
    let out = Iobuf.create ~len:0 in
    Iobuf.set_bounds_and_buffer ~src:t.data_view ~dst:out;
    Iobuf.flip_lo out;
    Writer.Expert.write_iobuf writer ~buf:(Iobuf.read_only out)
  ;;
end

module Parser_errors = struct
  type t =
    { mutable timestamp_too_large : int
    ; mutable invalid_size_on_record : int
    ; mutable invalid_string_ref : int
    ; mutable invalid_thread_ref : int
    ; mutable invalid_tick_initialization : int
    }
  [@@deriving sexp]

  let empty =
    { timestamp_too_large = 0
    ; invalid_size_on_record = 0
    ; invalid_string_ref = 0
    ; invalid_thread_ref = 0
    ; invalid_tick_initialization = 0
    }
  ;;

  let clear t =
    t.timestamp_too_large <- 0;
    t.invalid_size_on_record <- 0;
    t.invalid_string_ref <- 0;
    t.invalid_thread_ref <- 0;
    t.invalid_tick_initialization <- 0
  ;;

  let has_errors t =
    t.invalid_size_on_record > 0
    || t.invalid_string_ref > 0
    || t.invalid_thread_ref > 0
    || t.timestamp_too_large > 0
    || t.invalid_tick_initialization > 0
  ;;
end

type t =
  { writer : Writer.t
  ; record_writer : Record_writer.t
  ; parser : Parser.t
  ; errors : Parser_errors.t
  ; mutable empty : bool
  ; mutable remaining : Bytes.t option
  (* Store two checkpoints as a double buffer. When a dump is requested, we will
     always be able to output all of [prev], even if [current] was just started. *)
  ; mutable prev : Checkpoint.t
  ; mutable current : Checkpoint.t
  ; mutable size_bits : int
  ; state : State.t
  }

let process_record t record =
  (match record with
   | Parser.Record.Event _ -> t.empty <- false
   | _ -> ());
  State.process t.state record;
  Checkpoint.process t.current record;
  try
    Record_writer.write_record t.record_writer ~record;
    true
  with
  | Failure _ -> false
;;

let create ?num_temp_strs ~size_bits () =
  let prev = Checkpoint.create ~size_bits ~begin_state:(State.create ()) in
  let current = Checkpoint.create ~size_bits ~begin_state:(State.create ()) in
  let writer =
    Writer.Expert.create_no_header ?num_temp_strs ~destination:current.destination ()
  in
  let record_writer = Record_writer.create writer in
  { writer
  ; record_writer
  ; empty = true
  ; parser = Parser.create ()
  ; remaining = None
  ; errors = Parser_errors.empty
  ; prev
  ; current
  ; size_bits
  ; state = State.create ()
  }
;;

let flip t =
  Writer.Expert.flush t.writer;
  t.prev <- t.current;
  t.current <- Checkpoint.create ~size_bits:t.size_bits ~begin_state:(State.copy t.state);
  Writer.Expert.set_destination t.writer ~destination:t.current.destination
;;

let resize t ~size_bits =
  Writer.Expert.flush t.writer;
  t.size_bits <- size_bits;
  t.empty <- true;
  t.prev <- Checkpoint.create ~size_bits ~begin_state:(State.copy t.state);
  t.current <- Checkpoint.create ~size_bits ~begin_state:(State.copy t.state);
  Writer.Expert.set_destination t.writer ~destination:t.current.destination
;;

module Process_result = struct
  type t =
    | Complete
    | Incomplete of Bytes.t
    | Out_of_space of int
end

let process_until t data =
  let rec parse () =
    let at = Iobuf.Expert.lo data in
    match Parser.parse_next t.parser with
    | Ok record ->
      if process_record t record then parse () else Process_result.Out_of_space at
    | Error (Incomplete_record remaining) -> Incomplete remaining
    | Error No_more_words -> Complete
    | Error Timestamp_too_large ->
      t.errors.timestamp_too_large <- t.errors.timestamp_too_large + 1;
      parse ()
    | Error Invalid_size_on_record ->
      t.errors.invalid_size_on_record <- t.errors.invalid_size_on_record + 1;
      parse ()
    | Error Invalid_string_ref ->
      t.errors.invalid_string_ref <- t.errors.invalid_string_ref + 1;
      parse ()
    | Error Invalid_thread_ref ->
      t.errors.invalid_thread_ref <- t.errors.invalid_thread_ref + 1;
      parse ()
    | Error Invalid_tick_initialization ->
      t.errors.invalid_tick_initialization <- t.errors.invalid_tick_initialization + 1;
      parse ()
  in
  parse ()
;;

let rec consume'' t data =
  match process_until t data with
  | Process_result.Complete -> t.remaining <- None
  | Incomplete remaining -> t.remaining <- Some remaining
  | Out_of_space at ->
    Iobuf.Expert.set_lo data at;
    flip t;
    consume'' t data
;;

let consume' t data =
  let data =
    match t.remaining with
    | None -> data
    | Some prefix ->
      let buf = Iobuf.create ~len:(Bytes.length prefix + Iobuf.length data) in
      Iobuf.Fill.byteso buf prefix;
      Iobuf.Blit_fill.blito ~src:data ~dst:buf ();
      Iobuf.reset buf;
      Iobuf.read_only buf
  in
  Parser.set_buffer t.parser data;
  consume'' t data
;;

let consume t data =
  consume' t data;
  Parser_errors.clear t.errors
;;

let try_consume t data =
  consume' t data;
  let ret =
    if Parser_errors.has_errors t.errors
    then Or_error.error_s [%message (t.errors : Parser_errors.t)]
    else Ok ()
  in
  Parser_errors.clear t.errors;
  ret
;;

(* Writing a trace with only tick init + interning events is valid, but
   Perfetto will crash when interacting with it, so if the trace is empty we add
   one real event telling the user this. *)
let write_empty_trace_event writer =
  (* Overwriting string/thread slots doesn't matter; there are no other events. *)
  let name = Writer.set_temp_string_slot writer ~slot:0 "Empty trace!" in
  let thread = Writer.set_thread_slot writer ~slot:0 ~pid:1 ~tid:2 in
  let header =
    Writer.Expert.precompute_header
      ~event_type:Writer.Expert.Event_type.instant
      ~extra_words:0
      ~arg_types:Writer.Arg_types.none
      ~thread
      ~category:name
      ~name
  in
  Writer.Expert.write_from_header_with_tsc writer ~header
;;

let output t final_dest =
  Writer.Expert.flush t.writer;
  let references = Checkpoint.union_refs t.prev t.current in
  let state = State.filter t.prev.begin_state ~references in
  State.construct_trace state final_dest;
  Checkpoint.write t.prev final_dest;
  Checkpoint.write t.current final_dest;
  if t.empty then write_empty_trace_event final_dest
;;
