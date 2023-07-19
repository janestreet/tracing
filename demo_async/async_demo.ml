open! Core
open Async
open File_path.Operators

let[@trace.async
  "demo"
    "process"
    ~path:(Tracing_probes.Expert.intern_string (File_path.to_string path) : string)] rec process_directory
                                                                                           path
  =
  let%bind stat = Filesystem_async.stat path in
  [%trace.async_instant "demo" "stat"];
  let num_bytes = Int63.to_int stat.size |> Option.value ~default:0 in
  match stat.kind with
  | Regular ->
    print_endline [%string "File: %{path#File_path}, Size: %{num_bytes#Int} bytes"];
    return num_bytes
  | Directory ->
    print_endline [%string "Directory: %{path#File_path}\n"];
    let%bind files = Filesystem_async.ls_dir path in
    let%bind entry_sizes =
      Deferred.List.map ~how:(`Max_concurrent_jobs 10) files ~f:(fun file ->
        let res = process_directory (path /?. file) in
        res)
    in
    return (List.fold entry_sizes ~init:0 ~f:( + ))
  | _ -> return 0
;;

let main ~dir =
  let%map total_size = process_directory dir in
  print_endline [%string "\nTotal size: %{total_size#Int} bytes"]
;;

let filename =
  let pid = Core_unix.getpid () in
  (Lazy.force Filesystem_async.executable_name |> File_path.dirname_exn)
  /?/ ~/[%string "ppx-trace-%{pid#Pid}"]
;;

let () =
  Tracing_probes.Probes.(enable ~category:(Category.of_string "demo") ());
  Command_unix.run
    (Command.async
       ~summary:"write trace"
       ~behave_nicely_in_pipeline:true
       [%map_open.Command
         let () = Tracing_introspection.serve_param
         and () = Segment_remapper.param
         and dir =
           flag "-dir" (required File_path.arg_type) ~doc:"Directory to traverse"
         in
         fun () ->
           print_s [%message (filename : File_path.t)];
           Tracing_probes.start_with_sidecar
             ~launch:From_appdir
             ~filename:(filename :> string)
             ~write:All_events
             ~allow_gigatext:true
             ();
           let%bind () = main ~dir in
           let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
           return (Tracing_probes.close ())])
;;
