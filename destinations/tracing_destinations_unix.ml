open! Core

module type Destination = Tracing_zero.Writer_intf.Destination

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
  (module Dest : Destination)
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

let file_writer ?num_temp_strs ~filename () =
  let destination = file_destination ~filename () in
  Tracing_zero.Writer.Expert.create ?num_temp_strs ~destination ()
;;
