open Core
open Async

(* A child forked during an expect_test will remove the test output when it exits,
   so we instead exec the tests as separate executables. *)
let commander_exe = "sidecar_commander.exe"

let trace_to_buf ?num_temp_strs f =
  let buf = Iobuf.create ~len:100_000 in
  let destination = Tracing_zero.Destinations.iobuf_destination buf in
  let w = Tracing_zero.Writer.Expert.create ?num_temp_strs ~destination () in
  f w;
  Tracing_zero.Writer.close w;
  buf
;;

let run_with_tmp tmp_file args f =
  let open Deferred.Let_syntax in
  let cwd = Sys_unix.getcwd () in
  let args = [ "-file"; tmp_file ] @ args in
  let%bind process = Process.create_exn ~prog:(cwd ^/ commander_exe) ~args () in
  let%bind (_ : Process.Output.t) = Process.collect_output_and_wait process in
  f ();
  Sys.remove tmp_file
;;

let compare_output file expected =
  let stat = Core_unix.stat file in
  let len = Int.of_int64_exn stat.st_size in
  let data = Bytes.create len in
  let fd = Core_unix.openfile ~mode:[ O_RDONLY ] file in
  let read = Core_unix.read ~pos:0 ~len fd ~buf:data in
  Core_unix.close fd;
  Expect_test_helpers_core.require_equal [%here] (module Int) len read;
  let res_buf = Iobuf.of_bytes data in
  Expect_test_helpers_core.require_equal
    [%here]
    (module Int)
    (Iobuf.length expected)
    (Iobuf.length res_buf);
  Expect_test_patdiff.print_patdiff_s
    [%sexp (expected : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (res_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
;;

let%expect_test "sidecar_to_file" =
  let open Deferred.Let_syntax in
  let tmp_file = "sidecar_to_file_test" in
  let expected_buf = trace_to_buf Tracing_demo.write_demo_trace in
  let%bind () =
    run_with_tmp
      tmp_file
      [ "-to-file"; "-freestanding-binary"; "../sidecar/bin/main.exe" ]
      (fun () -> compare_output tmp_file expected_buf)
  in
  [%expect {| |}];
  Deferred.return ()
;;

let%expect_test "sidecar_buffer_to_file" =
  let open Deferred.Let_syntax in
  let tmp_file = "sidecar_buffer_to_file_test" in
  let trace_data =
    Trace_test_helpers.trace_to_buf (fun writer ->
      for _ = 1 to 10 do
        Tracing_demo.write_demo_trace writer
      done)
  in
  let expected_buf = Iobuf.create ~len:2048 in
  let (module Dest) = Tracing_zero.Destinations.iobuf_destination expected_buf in
  let num_temp_strs = 100 in
  let writer =
    Tracing_zero.Writer.Expert.create ~num_temp_strs ~destination:(module Dest) ()
  in
  let size_bits = 8 in
  let buffer = Tracing.Buffer.create ~num_temp_strs ~size_bits () in
  Tracing.Buffer.consume buffer (Iobuf.read_only trace_data);
  Tracing.Buffer.output buffer writer;
  Tracing_zero.Writer.close writer;
  let%bind () =
    run_with_tmp
      tmp_file
      [ "-buffer-to-file"
      ; Int.to_string size_bits
      ; "-freestanding-binary"
      ; "../sidecar/bin/main.exe"
      ]
      (fun () -> compare_output tmp_file expected_buf)
  in
  [%expect {| |}];
  Deferred.return ()
;;
