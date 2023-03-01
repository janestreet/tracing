(** Uses Quickcheck to generate round-trip tests to exercise the trace parser (via the
    high-level Trace API).

    In general, each type in this module corresponds to a type defined in the
    [Tracing.Parser] interface. *)
open! Core

module Generator = Base_quickcheck.Generator
module Record_writer = Tracing.Record_writer
module Parser = Tracing.Parser
module Trace = Tracing.Trace
module TW = Tracing_zero.Writer

let printable_string_gen = Generator.string_of Generator.char_print

module Thread = struct
  type t =
    { pname : (string[@quickcheck.generator printable_string_gen])
    ; tname : (string[@quickcheck.generator printable_string_gen])
    }
  [@@deriving sexp, quickcheck]
end

module Arg_value = struct
  type t =
    | Int of int
    | Float of (float[@quickcheck.generator Generator.float_without_nan])
    | String of (string[@quickcheck.generator printable_string_gen])
  [@@deriving sexp, quickcheck]
end

module Arg = struct
  type t = (string[@quickcheck.generator printable_string_gen]) * Arg_value.t
  [@@deriving sexp, quickcheck]
end

module Event_type = struct
  type t =
    | Instant
    | Counter
    | Duration
    | Duration_complete
    (* Generate between 1 and 5 flow steps. *)
    | Flow of (int[@quickcheck.generator Generator.int_inclusive 1 5])
  [@@deriving sexp, quickcheck]
end

module Event = struct
  type t =
    { event_type : Event_type.t
    ; name : (string[@quickcheck.generator printable_string_gen])
    ; category : (string[@quickcheck.generator printable_string_gen])
    ; thread : Thread.t (* Each event has exactly 5 arguments. *)
    ; args :
        (Arg.t list
         [@quickcheck.generator
           Generator.list_with_length Arg.quickcheck_generator ~length:5])
    }
  [@@deriving sexp, quickcheck]
end

(* This type represents the trace to be written. We generate a list of 50 events
   which will be converted into trace records. *)
module Trace_output = struct
  type t =
    (Event.t list
     [@quickcheck.generator
       Generator.list_with_length Event.quickcheck_generator ~length:50])
  [@@deriving sexp, quickcheck]
end

let to_writer_args (args : Arg.t list) : Trace.Arg.t list =
  List.map args ~f:(fun (name, arg_val) ->
    let value : Trace.Arg.value =
      match (arg_val : Arg_value.t) with
      | Int value -> Int value
      | Float value -> Float value
      | String value -> String value
    in
    name, value)
;;

(* Allocate a new pid and tid for each event. *)
let allocate_thread trace ~thread : Trace.Thread.t =
  let { Thread.pname; tname } = thread in
  let pid = Trace.allocate_pid trace ~name:pname in
  Trace.allocate_thread trace ~pid ~name:tname
;;

let write_events (trace : Trace.t) (events : Event.t list) =
  (* We increment [ticks] by 100 each time a record is written. *)
  let ticks = ref 0 in
  List.iter events ~f:(fun event ->
    let { Event.event_type; name; category; thread; args } = event in
    let args = to_writer_args args in
    let thread = allocate_thread trace ~thread in
    let time = Time_ns.Span.of_int_ns !ticks in
    (match event_type with
     | Instant -> Trace.write_instant trace ~args ~thread ~category ~name ~time
     | Counter ->
       (* Counters cannot have string arguments so we filter them out. *)
       let args =
         List.filter args ~f:(function
           | _, String _ -> false
           | _ -> true)
       in
       Trace.write_counter trace ~args ~thread ~category ~name ~time
     | Duration ->
       (* Write a duration begin event immediately followed by a duration end event
          100 ticks later. *)
       Trace.write_duration_begin trace ~args ~thread ~category ~name ~time;
       ticks := !ticks + 100;
       let time = Time_ns.Span.of_int_ns !ticks in
       Trace.write_duration_end trace ~args ~thread ~category ~name ~time
     | Duration_complete ->
       Trace.write_duration_instant trace ~args ~thread ~category ~name ~time
     | Flow num_steps ->
       (* Write [num_steps] flow step events, each separated by 100 ticks.

          This will generate flow events outside of duration events, which isn't
          semantically valid, but neither the parser nor the writer care so it's fine for
          this test. *)
       let flow = Trace.create_flow trace in
       for _ = 1 to num_steps do
         ticks := !ticks + 100;
         let time = Time_ns.Span.of_int_ns !ticks in
         Trace.write_flow_step trace flow ~thread ~time
       done;
       Trace.finish_flow trace flow);
    ticks := !ticks + 100)
;;

let write_expected_trace writer ~events =
  let trace = Trace.Expert.create ~base_time:(Some Time_ns.epoch) writer in
  write_events trace events
;;

let write_new_trace writer ~parser =
  let rec gen_trace parser record_writer =
    let record = Parser.parse_next parser in
    match record with
    | Ok record ->
      Record_writer.write_record record_writer ~record;
      gen_trace parser record_writer
    | Error _ -> ()
  in
  let record_writer = Record_writer.create writer in
  gen_trace parser record_writer
;;

let%test_unit "quickcheck round trips" =
  Quickcheck.test
    ~sexp_of:Trace_output.sexp_of_t
    Trace_output.quickcheck_generator
    ~f:(fun events ->
      let expected_buf = Trace_test_helpers.trace_to_buf (write_expected_trace ~events) in
      let expected_buf_str = Iobuf.to_string expected_buf in
      let parser = Tracing.Parser.create ~buffer:(Iobuf.read_only expected_buf) () in
      let num_temp_strs = TW.String_id.max_number_of_temp_string_slots in
      let new_buf =
        Trace_test_helpers.trace_to_buf ~num_temp_strs (write_new_trace ~parser)
      in
      let new_buf_str = Iobuf.to_string new_buf in
      [%test_result: string] new_buf_str ~expect:expected_buf_str)
;;
