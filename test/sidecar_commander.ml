open Core
open Tracing_sidecar

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"tracing sidecar test helper"
    [%map_open
      let file = flag "file" (required Command.Param.string) ~doc:"Output filename"
      and to_file = flag "to-file" no_arg ~doc:"Write to file"
      and forward_to_file = flag "forward-to-file" no_arg ~doc:"Forward to file"
      and buffer_to_file = flag "buffer-to-file" (optional int) ~doc:"INT Buffer to file"
      and freestanding_binary =
        flag
          "freestanding-binary"
          (optional string)
          ~doc:"PATH Write to file using freestanding sidecar"
      and freestanding_appdir =
        flag "freestanding-appdir" no_arg ~doc:"Use appdir deployed freestanding sidecar"
      and freestanding_exe_server =
        flag
          "freestanding-exe-server"
          no_arg
          ~doc:"Use exe-server deployed freestanding sidecar"
      in
      fun () ->
        let (output : Output.t), (write : Write.t), (launch : Launch.t), n =
          match
            ( to_file
            , forward_to_file
            , buffer_to_file
            , freestanding_binary
            , freestanding_appdir
            , freestanding_exe_server )
          with
          | true, false, None, None, false, false -> File file, All_events, By_forking, 1
          | false, true, None, None, false, false ->
            ( Forward (Tracing_destinations_unix.file_destination ~filename:file ())
            , All_events
            , By_forking
            , 1 )
          | false, false, Some buffer_size_bits, None, false, false ->
            File file, On_request { buffer_size_bits }, By_forking, 10
          | false, false, None, Some binary, false, false ->
            File file, All_events, From_binary binary, 1
          | false, false, None, None, true, false -> File file, All_events, From_appdir, 1
          | false, false, None, None, false, true ->
            File file, All_events, From_exe_server, 1
          | _ -> failwith "Bad arguments."
        in
        let sidecar = start (Config.create ~output ~write ~launch ()) in
        let destination = destination sidecar in
        let writer = Tracing_zero.Writer.Expert.create ~destination () in
        for _ = 1 to n do
          Tracing_demo.write_demo_trace writer
        done;
        Tracing_zero.Writer.close writer;
        (match buffer_to_file with
         | None -> ()
         | Some _ -> write_buffer sidecar);
        close sidecar]
;;

let () = Command_unix.run command
