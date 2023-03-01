open Core

let main file =
  let file_destination =
    Tracing_zero.Destinations.direct_file_destination ~filename:file ()
  in
  let sidecar =
    Tracing_sidecar.start
      (Tracing_sidecar.Config.create ~output:(Forward file_destination) ())
  in
  let destination = Tracing_sidecar.destination sidecar in
  let writer = Tracing_zero.Writer.Expert.create ~destination () in
  Tracing_demo.write_demo_trace writer;
  Tracing_zero.Writer.close writer;
  Tracing_sidecar.close sidecar
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"tracing sidecar test helper"
    [%map_open
      let file = flag "file" (required Command.Param.string) ~doc:"output filename" in
      fun () -> main file]
;;

let () = Command_unix.run command
