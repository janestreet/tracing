open! Core
open File_path.Operators

let process_directory dir_path =
  let[@trace "demo" "process" ~arg:(total_size : int)] rec process queue total_size =
    match Queue.dequeue queue with
    | None -> total_size
    | Some path ->
      let stat = Filesystem_core.stat path in
      [%trace.begin "demo" "manual"];
      let[@trace "demo" "auto"] () = Core_unix.sleep 1 in
      [%trace.end "demo" "manual"];
      (match stat.kind with
       | Regular ->
         let num_bytes = Int63.to_int stat.size |> Option.value ~default:0 in
         print_endline [%string "File: %{path#File_path}, Size: %{num_bytes#Int} bytes"];
         process queue (total_size + num_bytes)
       | Directory ->
         print_endline [%string "Directory: %{path#File_path}\n"];
         Filesystem_core.ls_dir path
         |> List.iter ~f:(fun file -> Queue.enqueue queue (path /?. file));
         process queue total_size
       | _ -> process queue total_size)
  in
  let queue = Queue.create () in
  Queue.enqueue queue dir_path;
  process queue 0
;;

let main ~dir =
  let total_size = process_directory dir in
  print_endline [%string "\nTotal size: %{total_size#Int} bytes"]
;;

let filename =
  let pid = Core_unix.getpid () in
  (Lazy.force Filesystem_core.executable_name |> File_path.dirname_exn)
  /?/ ~/[%string "ppx-trace-%{pid#Pid}"]
;;

let () =
  Command_unix.run
    (Command.basic
       ~summary:"write trace"
       [%map_open.Command
         let () = Tracing_introspection.serve_param
         and () = Segment_remapper.param
         and dir =
           flag "-dir" (required File_path.arg_type) ~doc:"Directory to traverse"
         in
         fun () ->
           Tracing_probes.Probes.(enable ~category:(Category.of_string "demo") ());
           print_s [%message (filename : File_path.t)];
           Tracing_probes.start_with_sidecar
             ~filename:(filename :> string)
             ~launch:From_appdir
             ~write:All_events
             ~allow_gigatext:true
             ();
           main ~dir;
           Tracing_probes.close ()])
;;
