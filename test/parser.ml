open! Core
module TW = Tracing_zero.Writer
module Parser = Tracing.Parser

(* Given an iobuf and the starting position of a word, overwrites the value of the bits
   from [field_pos] to [field_pos + field_size - 1]. *)
let write_field buf i ~word_pos ~field_pos ~field_size =
  let field_upper_bound = field_pos + field_size in
  let open Int64 in
  let word = Iobuf.Peek.int64_t_le buf ~pos:word_pos in
  let field_mask = (one lsl field_upper_bound) - (one lsl field_pos) in
  let word_with_cleared_field = word land lnot field_mask in
  (* set field *)
  Iobuf.Poke.int64_t_le
    buf
    ~pos:word_pos
    (word_with_cleared_field lor (of_int i lsl field_pos))
;;

let%expect_test "parses demo trace" =
  let buf = Trace_test_helpers.trace_to_buf Tracing_demo.write_demo_trace in
  let parser = Iobuf.read_only buf |> Parser.create in
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    {|
    (Ok (Interned_string (index 1) (value process)))
    (Ok
     (Tick_initialization (ticks_per_second 1000000000)
      (base_time ((1969-12-31 19:00:00.000000001-05:00)))))
    (Ok (Interned_string (index 102) (value myproc)))
    (Ok (Process_name_change (name 102) (pid 1)))
    (Ok (Interned_string (index 103) (value mythread)))
    (Ok (Thread_name_change (name 103) (pid 1) (tid 2)))
    (Ok
     (Interned_thread (index 1)
      (value ((pid 1) (tid 2) (process_name (myproc)) (thread_name (mythread))))))
    (Ok (Interned_string (index 104) (value stuff)))
    (Ok (Interned_string (index 105) (value my_funct)))
    (Ok
     (Event
      ((timestamp 10us) (thread 1) (category 104) (name 105)
       (arguments ((105 (String 104))))
       (event_type (Duration_complete (end_time 5ms))))))
    (Ok (Interned_string (index 3) (value wow)))
    (Ok (Interned_string (index 4) (value cool)))
    (Ok
     (Event
      ((timestamp 7ms) (thread 1) (category 104) (name 105)
       (arguments ((105 (String 3)) (104 (String 4))))
       (event_type (Duration_complete (end_time 15ms))))))
    (Ok
     (Event
      ((timestamp 2ms) (thread 1) (category 0) (name 0) (arguments ())
       (event_type (Flow_begin (flow_correlation_id 1))))))
    (Ok
     (Event
      ((timestamp 17ms) (thread 1) (category 104) (name 105) (arguments ())
       (event_type Instant))))
    (Ok
     (Event
      ((timestamp 20ms) (thread 1) (category 104) (name 105) (arguments ())
       (event_type Duration_begin))))
    (Ok
     (Event
      ((timestamp 15ms) (thread 1) (category 0) (name 0) (arguments ())
       (event_type (Flow_step (flow_correlation_id 1))))))
    (Ok
     (Event
      ((timestamp 40ms) (thread 1) (category 104) (name 105)
       (arguments
        ((105 (Int -4611686018427387904)) (104 (Int -1)) (102 (Float -1))))
       (event_type Duration_end))))
    (Ok
     (Event
      ((timestamp 20ms) (thread 1) (category 0) (name 0) (arguments ())
       (event_type (Flow_end (flow_correlation_id 1))))))
    (Ok
     (Event
      ((timestamp 7ms) (thread 1) (category 104) (name 105)
       (arguments ((104 (Int 1)))) (event_type (Counter (id 1))))))
    (Ok
     (Event
      ((timestamp 20ms) (thread 1) (category 104) (name 105)
       (arguments ((104 (Float 2.5)))) (event_type (Counter (id 1))))))
    (Error No_more_words)
    ((num_unparsed_records 0) (num_unparsed_args 0)) |}]
;;

let%expect_test "parser asks for more words before parsing record" =
  let write_trace writer =
    let thread = TW.set_thread_slot writer ~slot:0 ~pid:0 ~tid:0 in
    TW.write_instant
      writer
      ~arg_types:TW.Arg_types.none
      ~thread
      ~category:TW.String_id.empty
      ~name:TW.String_id.empty
      ~ticks:100
  in
  let buf = Trace_test_helpers.trace_to_buf write_trace in
  (* Removes the last 8 bytes from the iobuf, cutting off the last word in the instant
     event record. *)
  Iobuf.resize buf ~len:(Iobuf.length buf - 8);
  let parser = Iobuf.read_only buf |> Parser.create in
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    "\n\
    \    (Ok (Interned_string (index 1) (value process)))\n\
    \    (Ok\n\
    \     (Interned_thread (index 1)\n\
    \      (value ((pid 0) (tid 0) (process_name ()) (thread_name ())))))\n\
    \    (Error No_more_words)\n\
    \    ((num_unparsed_records 0) (num_unparsed_args 0))"];
  (* The first 8 bytes of the instant event record should be unparsed. *)
  [%test_result: int] (Iobuf.length buf) ~expect:8;
  (* After resetting the iobuf to include the last 8 bytes, the trace should parse with no
     errors. *)
  Iobuf.resize buf ~len:16;
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    "\n\
    \    (Ok\n\
    \     (Event\n\
    \      ((timestamp 100ns) (thread 1) (category 0) (name 0) (arguments ())\n\
    \       (event_type Instant))))\n\
    \    (Error No_more_words)\n\
    \    ((num_unparsed_records 0) (num_unparsed_args 0))"]
;;

let%expect_test "parser returns 'no more words' for invalid record" =
  (* Write a duration begin event containing only a header word and a timestamp.
     We set num_args = 1 so the parser expects more arguments word to follow, causing a
     parse error.

     The parser should still be able to parse the subsequent instant event record. *)
  let write_trace writer =
    let thread = TW.set_thread_slot writer ~slot:0 ~pid:0 ~tid:0 in
    TW.write_duration_begin
      writer
      ~arg_types:TW.Arg_types.none
      ~thread
      ~category:TW.String_id.empty
      ~name:TW.String_id.empty
      ~ticks:100;
    TW.write_instant
      writer
      ~arg_types:TW.Arg_types.none
      ~thread
      ~category:TW.String_id.empty
      ~name:TW.String_id.empty
      ~ticks:100
  in
  let buf = Trace_test_helpers.trace_to_buf write_trace in
  (* The duration begin event starts 32 bytes before the end of the buffer. *)
  let word_pos = Iobuf.length buf - 32 in
  (* Set num_args to 1 on the duration begin event. *)
  write_field buf 1 ~word_pos ~field_pos:20 ~field_size:4;
  let parser = Iobuf.read_only buf |> Parser.create in
  Trace_test_helpers.print_records_until_error parser;
  [%expect
    "\n\
    \    (Ok (Interned_string (index 1) (value process)))\n\
    \    (Ok\n\
    \     (Interned_thread (index 1)\n\
    \      (value ((pid 0) (tid 0) (process_name ()) (thread_name ())))))\n\
    \    (Error Invalid_size_on_record)"];
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    "\n\
    \    (Ok\n\
    \     (Event\n\
    \      ((timestamp 100ns) (thread 1) (category 0) (name 0) (arguments ())\n\
    \       (event_type Instant))))\n\
    \    (Error No_more_words)\n\
    \    ((num_unparsed_records 1) (num_unparsed_args 0))"]
;;

let%expect_test "parsing returns invalid thread ref" =
  (* Write an Instant event with thread index = 42 before registering a thread with
     with index = 42 into the thread table, causing a parse error. *)
  let write_trace writer =
    let thread = TW.set_thread_slot writer ~slot:0 ~pid:0 ~tid:0 in
    TW.write_instant
      writer
      ~arg_types:TW.Arg_types.none
      ~thread
      ~category:TW.String_id.empty
      ~name:TW.String_id.empty
      ~ticks:100
  in
  let buf = Trace_test_helpers.trace_to_buf write_trace in
  (* The instant event starts 16 bytes before the end of the buffer. *)
  let word_pos = Iobuf.length buf - 16 in
  (* Set thread_index to 42 on the event. *)
  write_field buf 42 ~word_pos ~field_pos:24 ~field_size:8;
  let parser = Iobuf.read_only buf |> Parser.create in
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    "\n\
    \    (Ok (Interned_string (index 1) (value process)))\n\
    \    (Ok\n\
    \     (Interned_thread (index 1)\n\
    \      (value ((pid 0) (tid 0) (process_name ()) (thread_name ())))))\n\
    \    (Error Invalid_thread_ref)\n\
    \    ((num_unparsed_records 1) (num_unparsed_args 0))"]
;;

let%expect_test "parsing returns invalid string ref" =
  (* Write an Instant event where the name references string_index = 23 before that string
     index was set, causing a parse error. *)
  let write_trace writer =
    let thread = TW.set_thread_slot writer ~slot:0 ~pid:0 ~tid:0 in
    let str = TW.set_temp_string_slot writer ~slot:0 "test" in
    TW.write_instant
      writer
      ~arg_types:TW.Arg_types.none
      ~thread
      ~category:TW.String_id.empty
      ~name:str
      ~ticks:100
  in
  let buf = Trace_test_helpers.trace_to_buf write_trace in
  (* The instant event starts 16 bytes before the end of the buffer. *)
  let word_pos = Iobuf.length buf - 16 in
  (* Set the string index of the "name" field to 23 on the event. *)
  write_field buf 23 ~word_pos ~field_pos:32 ~field_size:16;
  let parser = Iobuf.read_only buf |> Parser.create in
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    "\n\
    \    (Ok (Interned_string (index 1) (value process)))\n\
    \    (Ok\n\
    \     (Interned_thread (index 1)\n\
    \      (value ((pid 0) (tid 0) (process_name ()) (thread_name ())))))\n\
    \    (Ok (Interned_string (index 2) (value test)))\n\
    \    (Error Invalid_string_ref)\n\
    \    ((num_unparsed_records 1) (num_unparsed_args 0))"]
;;

let%expect_test "ignores unknown arg types" =
  (* Writes a counter event with two arguments: one has type 15 (not defined in the spec
     and should be skipped) and one int32 arg with value = 9. *)
  let write_trace writer =
    let thread = TW.set_thread_slot writer ~slot:0 ~pid:0 ~tid:0 in
    let arg_types = TW.Arg_types.create ~int32s:1 ~int64s:1 () in
    TW.write_counter
      writer
      ~arg_types
      ~thread
      ~category:TW.String_id.empty
      ~name:TW.String_id.empty
      ~ticks:100
      ~counter_id:3;
    TW.Write_arg.int64 writer ~name:TW.String_id.empty 12345678;
    TW.Write_arg.int32 writer ~name:TW.String_id.empty 9
  in
  let buf = Trace_test_helpers.trace_to_buf write_trace in
  (* The counter event starts 32 bytes before the end of the buffer. *)
  let word_pos = Iobuf.length buf - 32 in
  (* Change the first counter argument to have argtype = 15 (an unknown arg type). *)
  write_field buf 15 ~word_pos ~field_pos:0 ~field_size:4;
  let parser = Iobuf.read_only buf |> Parser.create in
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    "\n\
    \    (Ok (Interned_string (index 1) (value process)))\n\
    \    (Ok\n\
    \     (Interned_thread (index 1)\n\
    \      (value ((pid 0) (tid 0) (process_name ()) (thread_name ())))))\n\
    \    (Ok\n\
    \     (Event\n\
    \      ((timestamp 100ns) (thread 1) (category 0) (name 0)\n\
    \       (arguments ((0 (Int 9)))) (event_type (Counter (id 3))))))\n\
    \    (Error No_more_words)\n\
    \    ((num_unparsed_records 0) (num_unparsed_args 1))"]
;;

let%expect_test "parsing returns invalid tick initialization" =
  (* Should return a parse error if ticks per second is <= 0. *)
  let write_trace writer =
    let tick_translation = { TW.Tick_translation.epoch_ns with ticks_per_second = 0 } in
    TW.write_tick_initialization writer tick_translation
  in
  let buf = Trace_test_helpers.trace_to_buf write_trace in
  let parser = Iobuf.read_only buf |> Parser.create in
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    "\n\
    \    (Ok (Interned_string (index 1) (value process)))\n\
    \    (Error Invalid_tick_initialization)\n\
    \    ((num_unparsed_records 1) (num_unparsed_args 0))"]
;;

let%expect_test "parsing valid ticks returns timestamp too large" =
  (* Starts a trace where 1 tick = 1 second then writes an instant event where [ticks] is
     10^12. This timestamp corresponds to 10^21 nanoseconds which is too large to fit in
     a [Time_ns.Span.t]. The parser should return a parse error. *)
  let write_trace writer =
    let tick_translation = { TW.Tick_translation.epoch_ns with ticks_per_second = 1 } in
    TW.write_tick_initialization writer tick_translation;
    let thread = TW.set_thread_slot writer ~slot:0 ~pid:0 ~tid:0 in
    TW.write_instant
      writer
      ~arg_types:TW.Arg_types.none
      ~thread
      ~category:TW.String_id.empty
      ~name:TW.String_id.empty
      ~ticks:1_000_000_000_000
  in
  let buf = Trace_test_helpers.trace_to_buf write_trace in
  let parser = Iobuf.read_only buf |> Parser.create in
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    "\n\
    \    (Ok (Interned_string (index 1) (value process)))\n\
    \    (Ok\n\
    \     (Tick_initialization (ticks_per_second 1)\n\
    \      (base_time ((1969-12-31 19:00:00.000000000-05:00)))))\n\
    \    (Ok\n\
    \     (Interned_thread (index 1)\n\
    \      (value ((pid 0) (tid 0) (process_name ()) (thread_name ())))))\n\
    \    (Error Timestamp_too_large)\n\
    \    ((num_unparsed_records 1) (num_unparsed_args 0))"]
;;

let%expect_test "parsing negative ticks returns timestamp too large" =
  (* The writer sums two large, positive tick values and the result overflows into a
     negative number. The writer ends up writing a negative tick value. When the negative
     tick value is read, the parser should return a "timestamp too large" error. *)
  let write_trace writer =
    let thread = TW.set_thread_slot writer ~slot:0 ~pid:0 ~tid:0 in
    let valid_ticks1 = (1 lsl 61) + 5 in
    let valid_ticks2 = (1 lsl 61) + 6 in
    let overflowed_ticks = valid_ticks1 + valid_ticks2 in
    [%test_result: int] overflowed_ticks ~expect:(-4611686018427387893);
    TW.write_instant
      writer
      ~arg_types:TW.Arg_types.none
      ~thread
      ~category:TW.String_id.empty
      ~name:TW.String_id.empty
      ~ticks:overflowed_ticks
  in
  let buf = Trace_test_helpers.trace_to_buf write_trace in
  let parser = Iobuf.read_only buf |> Parser.create in
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    "\n\
    \    (Ok (Interned_string (index 1) (value process)))\n\
    \    (Ok\n\
    \     (Interned_thread (index 1)\n\
    \      (value ((pid 0) (tid 0) (process_name ()) (thread_name ())))))\n\
    \    (Error Timestamp_too_large)\n\
    \    ((num_unparsed_records 1) (num_unparsed_args 0))"]
;;

let%expect_test "parses large timestamps with nanosecond precision" =
  (* Write an event where 1 tick = 1 nanosecond (the default tick per second value) with
     [ticks] = 1.7e18 + 3. This timestamp is a similar order of magnitude as what would
     be returned by [Time_ns.now]. The test checks that we don't lose precision when using
     large tick values. *)
  let write_trace writer =
    let thread = TW.set_thread_slot writer ~slot:0 ~pid:0 ~tid:0 in
    TW.write_instant
      writer
      ~arg_types:TW.Arg_types.none
      ~thread
      ~category:TW.String_id.empty
      ~name:TW.String_id.empty
      ~ticks:1_700_000_000_000_000_003
  in
  let buf = Trace_test_helpers.trace_to_buf write_trace in
  let parser = Iobuf.read_only buf |> Parser.create in
  Trace_test_helpers.print_records_until_error parser;
  print_s [%sexp (Parser.warnings parser : Parser.Warnings.t)];
  [%expect
    "\n\
    \    (Ok (Interned_string (index 1) (value process)))\n\
    \    (Ok\n\
    \     (Interned_thread (index 1)\n\
    \      (value ((pid 0) (tid 0) (process_name ()) (thread_name ())))))\n\
    \    (Ok\n\
    \     (Event\n\
    \      ((timestamp 19675d22h13m20.000000003s) (thread 1) (category 0) (name 0)\n\
    \       (arguments ()) (event_type Instant))))\n\
    \    (Error No_more_words)\n\
    \    ((num_unparsed_records 0) (num_unparsed_args 0))"]
;;
