open Core
open Tracing_sidecar

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"tracing sidecar test helper"
    [%map_open
      let file = flag "file" (required Command.Param.string) ~doc:"Output filename"
      and to_file = flag "to-file" no_arg ~doc:"Write to file"
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
        let output : Output.t =
          match to_file, buffer_to_file with
          | false, Some _ | true, None -> File file
          | _ -> failwith "Bad arguments."
        in
        let (write : Write.t), n =
          match buffer_to_file with
          | None -> All_events, 1
          | Some i -> On_request { buffer_size_bits = i }, 10
        in
        let launch : Launch.t =
          match freestanding_binary, freestanding_appdir, freestanding_exe_server with
          | Some binary, false, false -> From_binary binary
          | None, true, false -> From_appdir
          | None, false, true -> From_exe_server
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
