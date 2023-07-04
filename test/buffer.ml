open Core
module B = Tracing.Buffer

let%expect_test "buffer output preserves data" =
  let num_temp_strs = 100 in
  let expected_buf =
    Trace_test_helpers.trace_to_buf ~num_temp_strs Tracing_demo.write_demo_trace
  in
  let target_buf0 = Iobuf.create ~len:(Iobuf.length expected_buf) in
  let target_buf1 = Iobuf.create ~len:(Iobuf.length expected_buf) in
  let (module Dest0) = Tracing_zero.Destinations.iobuf_destination target_buf0 in
  let (module Dest1) = Tracing_zero.Destinations.iobuf_destination target_buf1 in
  let writer0 =
    Tracing_zero.Writer.Expert.create ~num_temp_strs ~destination:(module Dest0) ()
  in
  let writer1 =
    Tracing_zero.Writer.Expert.create ~num_temp_strs ~destination:(module Dest1) ()
  in
  let buffer = B.create ~num_temp_strs ~size_bits:10 () in
  B.consume buffer (Iobuf.read_only (Iobuf.sub_shared expected_buf));
  B.output buffer writer0;
  B.output buffer writer1;
  Tracing_zero.Writer.close writer0;
  Tracing_zero.Writer.close writer1;
  printf
    "expected size %d, got sizes %d and %d\n"
    (Iobuf.length expected_buf)
    (Iobuf.length target_buf0)
    (Iobuf.length target_buf1);
  Expect_test_patdiff.print_patdiff_s
    [%sexp (expected_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (target_buf0 : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  Expect_test_patdiff.print_patdiff_s
    [%sexp (expected_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (target_buf1 : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {| expected size 832, got sizes 832 and 832 |}]
;;

let%expect_test "buffer one side" =
  let num_temp_strs = 100 in
  let expected_buf =
    Trace_test_helpers.trace_to_buf ~num_temp_strs Tracing_demo.write_demo_trace
  in
  let target_buf = Iobuf.create ~len:(Iobuf.length expected_buf) in
  let (module Dest) = Tracing_zero.Destinations.iobuf_destination target_buf in
  let writer =
    Tracing_zero.Writer.Expert.create ~num_temp_strs ~destination:(module Dest) ()
  in
  let buffer = B.create ~num_temp_strs ~size_bits:10 () in
  B.consume buffer (Iobuf.read_only (Iobuf.sub_shared expected_buf));
  B.output buffer writer;
  Tracing_zero.Writer.close writer;
  printf
    "expected size %d, got size %d\n"
    (Iobuf.length expected_buf)
    (Iobuf.length target_buf);
  Expect_test_patdiff.print_patdiff_s
    [%sexp (expected_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (target_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {| expected size 832, got size 832 |}]
;;

let%expect_test "buffer two sides" =
  let num_temp_strs = 100 in
  let expected_buf =
    Trace_test_helpers.trace_to_buf ~num_temp_strs Tracing_demo.write_demo_trace
  in
  let target_buf = Iobuf.create ~len:(Iobuf.length expected_buf) in
  let (module Dest) = Tracing_zero.Destinations.iobuf_destination target_buf in
  let writer =
    Tracing_zero.Writer.Expert.create ~num_temp_strs ~destination:(module Dest) ()
  in
  let buffer = B.create ~num_temp_strs ~size_bits:9 () in
  B.consume buffer (Iobuf.read_only (Iobuf.sub_shared expected_buf));
  B.output buffer writer;
  Tracing_zero.Writer.close writer;
  printf
    "expected size %d, got size %d\n"
    (Iobuf.length expected_buf)
    (Iobuf.length target_buf);
  Expect_test_patdiff.print_patdiff_s
    [%sexp (expected_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (target_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {| expected size 832, got size 832 |}]
;;

let%expect_test "buffer preserves required state after dropping events" =
  let num_temp_strs = 100 in
  let target_buf = Iobuf.create ~len:2048 in
  let trace_data =
    Trace_test_helpers.trace_to_buf (fun writer ->
      for _ = 1 to 10 do
        Tracing_demo.write_demo_trace writer
      done)
  in
  let (module Dest) = Tracing_zero.Destinations.iobuf_destination target_buf in
  let writer =
    Tracing_zero.Writer.Expert.create ~num_temp_strs ~destination:(module Dest) ()
  in
  let buffer = B.create ~num_temp_strs ~size_bits:8 () in
  B.consume buffer (Iobuf.read_only trace_data);
  B.output buffer writer;
  Tracing_zero.Writer.close writer;
  let parser = Tracing.Parser.create ~buffer:(Iobuf.read_only target_buf) () in
  (* This duplicates several string interning events because they're repeated in the
     actual event stream: we're just running the demo trace 10 times.
     Hence both the current state and the buffered trace data include the events.
     This won't happen in normal usage. *)
  Trace_test_helpers.print_records_until_error parser;
  [%expect
    {|
   (Ok (Interned_string (index 1) (value process)))
   (Ok
    (Tick_initialization (ticks_per_second 1000000000)
     (base_time ((1969-12-31 19:00:00.000000001-05:00)))))
   (Ok (Interned_string (index 157) (value stuff)))
   (Ok (Interned_string (index 158) (value my_funct)))
   (Ok
    (Interned_thread (index 1)
     (value ((pid 1) (tid 2) (process_name ()) (thread_name ())))))
   (Ok
    (Event
     ((timestamp 26ms) (thread 1) (category 157) (name 158)
      (arguments ((157 (Int 2))))
      (event_type (Async_instant (async_correlation_id 1))))))
   (Ok
    (Event
     ((timestamp 20ms) (thread 1) (category 0) (name 0) (arguments ())
      (event_type (Flow_end (flow_correlation_id 1))))))
   (Ok
    (Event
     ((timestamp 7ms) (thread 1) (category 157) (name 158)
      (arguments ((157 (Int 1)))) (event_type (Counter (id 1))))))
   (Ok
    (Event
     ((timestamp 29ms) (thread 1) (category 157) (name 158)
      (arguments ((157 (Int 2))))
      (event_type (Async_end (async_correlation_id 1))))))
   (Ok
    (Event
     ((timestamp 31ms) (thread 1) (category 157) (name 158)
      (arguments ((157 (Int 1))))
      (event_type (Async_end (async_correlation_id 2))))))
   (Ok
    (Event
     ((timestamp 20ms) (thread 1) (category 157) (name 158)
      (arguments ((157 (Float 2.5)))) (event_type (Counter (id 1))))))
   (Ok
    (Event
     ((timestamp 20ms) (thread 1) (category 157) (name 158)
      (arguments ((157 (Int64 9223372036854775807))))
      (event_type (Counter (id 1))))))
   (Ok
    (Event
     ((timestamp 20ms) (thread 1) (category 157) (name 158)
      (arguments ((157 (Pointer 0x7fffffffffffffff))))
      (event_type (Counter (id 1))))))
   (Error No_more_words) |}]
;;

let%expect_test "buffer resize" =
  let num_temp_strs = 100 in
  let target_buf = Iobuf.create ~len:2048 in
  let expected_buf =
    Trace_test_helpers.trace_to_buf ~num_temp_strs Tracing_demo.write_demo_trace
  in
  let trace_data = Trace_test_helpers.trace_to_buf Tracing_demo.write_demo_trace in
  let (module Dest) = Tracing_zero.Destinations.iobuf_destination target_buf in
  let writer =
    Tracing_zero.Writer.Expert.create ~num_temp_strs ~destination:(module Dest) ()
  in
  let buffer = B.create ~num_temp_strs ~size_bits:8 () in
  B.resize buffer ~size_bits:12;
  B.consume buffer (Iobuf.read_only (Iobuf.sub_shared trace_data));
  B.output buffer writer;
  Tracing_zero.Writer.close writer;
  printf
    "expected size %d, got size %d\n"
    (Iobuf.length expected_buf)
    (Iobuf.length target_buf);
  Expect_test_patdiff.print_patdiff_s
    [%sexp (expected_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (target_buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {|
    expected size 832, got size 832 |}]
;;

let%expect_test "buffer resize clears data but leaves state" =
  let num_temp_strs = 100 in
  let target_buf = Iobuf.create ~len:2048 in
  let expected_buf =
    Trace_test_helpers.trace_to_buf ~num_temp_strs Tracing_demo.write_demo_trace
  in
  let trace_data = Trace_test_helpers.trace_to_buf Tracing_demo.write_demo_trace in
  let (module Dest) = Tracing_zero.Destinations.iobuf_destination target_buf in
  let writer =
    Tracing_zero.Writer.Expert.create ~num_temp_strs ~destination:(module Dest) ()
  in
  let buffer = B.create ~num_temp_strs ~size_bits:8 () in
  B.consume buffer (Iobuf.read_only (Iobuf.sub_shared trace_data));
  B.resize buffer ~size_bits:12;
  B.consume buffer (Iobuf.read_only (Iobuf.sub_shared trace_data));
  B.output buffer writer;
  Tracing_zero.Writer.close writer;
  printf
    "expected size %d, got size %d\n"
    (Iobuf.length expected_buf)
    (Iobuf.length target_buf);
  let parser = Tracing.Parser.create ~buffer:(Iobuf.read_only target_buf) () in
  (* The output is slightly larger than the expected buf for a single demo
     trace because the buffer remembers state from the first demo trace.
     Hence some string interning events are output both from the old state
     and from newly recorded (second) data stream. *)
  Trace_test_helpers.print_records_until_error parser;
  [%expect
    {|
    expected size 832, got size 1032
    (Ok (Interned_string (index 1) (value process)))
    (Ok
     (Tick_initialization (ticks_per_second 1000000000)
      (base_time ((1969-12-31 19:00:00.000000001-05:00)))))
    (Ok (Interned_string (index 20) (value wow)))
    (Ok (Interned_string (index 21) (value cool)))
    (Ok (Interned_string (index 120) (value mythread)))
    (Ok (Interned_string (index 122) (value my_funct)))
    (Ok (Interned_string (index 119) (value myproc)))
    (Ok (Interned_string (index 121) (value stuff)))
    (Ok
     (Interned_thread (index 1)
      (value ((pid 1) (tid 2) (process_name ()) (thread_name ())))))
    (Ok (Thread_name_change (name 120) (pid 1) (tid 2)))
    (Ok (Process_name_change (name 119) (pid 1)))
    (Ok
     (Tick_initialization (ticks_per_second 1000000000)
      (base_time ((1969-12-31 19:00:00.000000001-05:00)))))
    (Ok (Interned_string (index 119) (value myproc)))
    (Ok (Process_name_change (name 119) (pid 1)))
    (Ok (Interned_string (index 120) (value mythread)))
    (Ok (Thread_name_change (name 120) (pid 1) (tid 2)))
    (Ok
     (Interned_thread (index 1)
      (value ((pid 1) (tid 2) (process_name (myproc)) (thread_name (mythread))))))
    (Ok (Interned_string (index 121) (value stuff)))
    (Ok (Interned_string (index 122) (value my_funct)))
    (Ok
     (Event
      ((timestamp 10us) (thread 1) (category 121) (name 122)
       (arguments ((122 (String 121))))
       (event_type (Duration_complete (end_time 5ms))))))
    (Ok (Interned_string (index 20) (value wow)))
    (Ok (Interned_string (index 21) (value cool)))
    (Ok
     (Event
      ((timestamp 7ms) (thread 1) (category 121) (name 122)
       (arguments ((122 (String 20)) (121 (String 21))))
       (event_type (Duration_complete (end_time 15ms))))))
    (Ok
     (Event
      ((timestamp 23ms) (thread 1) (category 121) (name 122)
       (arguments ((121 (Int 2))))
       (event_type (Async_begin (async_correlation_id 1))))))
    (Ok
     (Event
      ((timestamp 2ms) (thread 1) (category 0) (name 0) (arguments ())
       (event_type (Flow_begin (flow_correlation_id 1))))))
    (Ok
     (Event
      ((timestamp 17ms) (thread 1) (category 121) (name 122) (arguments ())
       (event_type Instant))))
    (Ok
     (Event
      ((timestamp 24ms) (thread 1) (category 121) (name 122)
       (arguments ((121 (Int 1))))
       (event_type (Async_begin (async_correlation_id 2))))))
    (Ok
     (Event
      ((timestamp 20ms) (thread 1) (category 121) (name 122) (arguments ())
       (event_type Duration_begin))))
    (Ok
     (Event
      ((timestamp 15ms) (thread 1) (category 0) (name 0) (arguments ())
       (event_type (Flow_step (flow_correlation_id 1))))))
    (Ok
     (Event
      ((timestamp 40ms) (thread 1) (category 121) (name 122)
       (arguments
        ((122 (Int -4611686018427387904)) (121 (Int -1)) (119 (Float -1))))
       (event_type Duration_end))))
    (Ok
     (Event
      ((timestamp 25ms) (thread 1) (category 121) (name 122)
       (arguments ((121 (Int 1))))
       (event_type (Async_instant (async_correlation_id 2))))))
    (Ok
     (Event
      ((timestamp 26ms) (thread 1) (category 121) (name 122)
       (arguments ((121 (Int 2))))
       (event_type (Async_instant (async_correlation_id 1))))))
    (Ok
     (Event
      ((timestamp 20ms) (thread 1) (category 0) (name 0) (arguments ())
       (event_type (Flow_end (flow_correlation_id 1))))))
    (Ok
     (Event
      ((timestamp 7ms) (thread 1) (category 121) (name 122)
       (arguments ((121 (Int 1)))) (event_type (Counter (id 1))))))
    (Ok
     (Event
      ((timestamp 29ms) (thread 1) (category 121) (name 122)
       (arguments ((121 (Int 2))))
       (event_type (Async_end (async_correlation_id 1))))))
    (Ok
     (Event
      ((timestamp 31ms) (thread 1) (category 121) (name 122)
       (arguments ((121 (Int 1))))
       (event_type (Async_end (async_correlation_id 2))))))
    (Ok
     (Event
      ((timestamp 20ms) (thread 1) (category 121) (name 122)
       (arguments ((121 (Float 2.5)))) (event_type (Counter (id 1))))))
    (Ok
     (Event
      ((timestamp 20ms) (thread 1) (category 121) (name 122)
       (arguments ((121 (Int64 9223372036854775807))))
       (event_type (Counter (id 1))))))
    (Ok
     (Event
      ((timestamp 20ms) (thread 1) (category 121) (name 122)
       (arguments ((121 (Pointer 0x7fffffffffffffff))))
       (event_type (Counter (id 1))))))
    (Error No_more_words) |}]
;;

let%expect_test "empty buffer outputs one event" =
  let num_temp_strs = 100 in
  let target_buf = Iobuf.create ~len:1024 in
  let (module Dest) = Tracing_zero.Destinations.iobuf_destination target_buf in
  let writer =
    Tracing_zero.Writer.Expert.create ~num_temp_strs ~destination:(module Dest) ()
  in
  let buffer = B.create ~num_temp_strs ~size_bits:8 () in
  B.output buffer writer;
  Tracing_zero.Writer.close writer;
  let parser = Tracing.Parser.create ~buffer:(Iobuf.read_only target_buf) () in
  let rec find_events () =
    let event = Tracing.Parser.parse_next parser in
    (match event with
     | Ok (Event _) -> printf "Got event\n"
     | _ -> ());
    if Result.is_ok event then find_events ()
  in
  find_events ();
  [%expect {| Got event |}]
;;
