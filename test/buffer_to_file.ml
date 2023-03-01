open Core

let main file =
  let sidecar =
    Tracing_sidecar.start
      (Tracing_sidecar.Config.create ~output:(File file) ~buffer_size_bits:8 ())
  in
  let destination = Tracing_sidecar.destination sidecar in
  let writer = Tracing_zero.Writer.Expert.create ~destination () in
  for _ = 1 to 10 do
    Tracing_demo.write_demo_trace writer
  done;
  Tracing_zero.Writer.close writer;
  Tracing_sidecar.write_buffer sidecar;
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
