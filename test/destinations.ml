open! Core
module Buffer_until_initialized = Tracing_zero.Destinations.Buffer_until_initialized

let%expect_test "Buffer_until_initialized" =
  (* Writing the demo trace 100 times hopefully lets us test the case of getting a new
     buffer more than once. *)
  let repeated_demo n writer =
    for _ = 1 to n do
      Tracing_demo.write_demo_trace writer
    done
  in
  (* We want to write the demo trace enough times before initializing that it exercises
     the path with multiple temp buffers. *)
  let before_n, after_n = 4, 2 in
  let expected_buf =
    Trace_test_helpers.trace_to_buf (repeated_demo (before_n + after_n))
  in
  let buffer = Buffer_until_initialized.create () in
  let destination = Buffer_until_initialized.to_destination buffer in
  let writer = Tracing_zero.Writer.Expert.create ~destination () in
  repeated_demo before_n writer;
  let res_buf = Iobuf.create ~len:100_000 in
  let buf_dest = Tracing_zero.Destinations.iobuf_destination res_buf in
  Buffer_until_initialized.set_destination buffer buf_dest;
  repeated_demo after_n writer;
  Tracing_zero.Writer.close writer;
  (* Writes after close should be gracefully ignored *)
  Tracing_demo.write_demo_trace writer;
  Expect_test_patdiff.print_patdiff_s
    [%sexp (expected_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (res_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {||}]
;;
