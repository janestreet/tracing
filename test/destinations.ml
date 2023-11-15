open! Core
module Buffer_until_initialized = Tracing_zero.Destinations.Buffer_until_initialized

let repeated_demo n writer =
  for _ = 1 to n do
    Tracing_demo.write_demo_trace writer
  done
;;

let make_ring file bits =
  let filename_prefix = Filename.temp_dir_name ^/ file in
  let filename = Ringbuf.create_mkstemp ~size_bits:bits ~filename_prefix in
  let producer = Ringbuf.Producer.of_file ~prefault:false filename in
  let consumer = Ringbuf.Consumer.of_file ~prefault:false filename in
  producer, consumer
;;

let%expect_test "Buffer_until_initialized" =
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

let%expect_test "file_descriptor" =
  let expected_buf = Trace_test_helpers.trace_to_buf Tracing_demo.write_demo_trace in
  let filename, fd = Filename_unix.open_temp_file_fd "fd_dest_test" "ftf" in
  let destination = Tracing_destinations_unix.fd_destination ~fd () in
  let writer = Tracing_zero.Writer.Expert.create ~destination () in
  Tracing_demo.write_demo_trace writer;
  Tracing_zero.Writer.close writer;
  Core_unix.close fd;
  let stat = Core_unix.stat filename in
  let len = Int.of_int64_exn stat.st_size in
  let data = Bytes.create len in
  let fd = Core_unix.openfile ~mode:[ O_RDONLY ] filename in
  let read = Core_unix.read ~pos:0 ~len fd ~buf:data in
  Core_unix.close fd;
  Expect_test_helpers_core.require_equal [%here] (module Int) len read;
  let res_buf = Iobuf.of_bytes data in
  Expect_test_patdiff.print_patdiff_s
    [%sexp (expected_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (res_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {||}]
;;

let%expect_test "ringbuf_blocking" =
  let producer, consumer = make_ring "ringbuf_test" 14 in
  let expected_buf = Trace_test_helpers.trace_to_buf (repeated_demo 10) in
  let destination =
    Tracing_destinations_unix.ringbuf_destination ~blocking:true producer
  in
  let writer = Tracing_zero.Writer.Expert.create ~destination () in
  repeated_demo 10 writer;
  Tracing_zero.Writer.close writer;
  let res_buf = Ringbuf.Consumer.poll consumer in
  Expect_test_patdiff.print_patdiff_s
    [%sexp (expected_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (res_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {||}]
;;

let%expect_test "ringbuf_non_blocking" =
  let bits = 12 in
  let producer, consumer = make_ring "ringbuf_test" bits in
  let len = Int.pow 2 bits in
  let expected_buf = Iobuf.create ~len in
  for _ = 0 to len - 1 do
    Iobuf.Fill.char expected_buf 'a'
  done;
  Iobuf.flip_lo expected_buf;
  let (module D) =
    Tracing_destinations_unix.ringbuf_destination ~blocking:false producer
  in
  (* Record 'a' data to fill the buffer *)
  for _ = 0 to len - 1 do
    let buf = D.next_buf ~ensure_capacity:1 in
    Iobuf.Fill.char buf 'a'
  done;
  (* Drop 'b' data *)
  for _ = 0 to len - 1 do
    let buf = D.next_buf ~ensure_capacity:1 in
    Iobuf.Fill.char buf 'b'
  done;
  D.close ();
  let res_buf = Ringbuf.Consumer.poll consumer in
  Expect_test_patdiff.print_patdiff_s
    [%sexp (expected_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (res_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {||}]
;;
