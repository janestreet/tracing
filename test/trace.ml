open! Core
module Trace = Tracing.Trace

let%expect_test "compare demo trace written with helper" =
  let raw_demo = Trace_test_helpers.trace_to_buf Tracing_demo.write_demo_trace in
  let helper_demo =
    Trace_test_helpers.trace_to_buf Tracing_demo.write_demo_trace_high_level
  in
  Expect_test_patdiff.print_patdiff_s
    [%sexp (raw_demo : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (helper_demo : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {| |}]
;;

(* Use 5x as much code as is being tested to test the behavior of the wrapper when there's
   more threads used than there are slots, which doesn't get exercised by the dominodb
   example so without this test would possibly get released without even having been
   executed.

   It does this by writing a scenario with many threads using the [Tracing.Trace] wrapper,
   then directly writing what that should result in using the [Tracing_zero] library and
   comparing the results. *)

(* Needs to be larger than 255 for wrapping behavior to be exhibited and test to fully
   pass, but you can set it smaller to make it easier to debug a differing result. *)
let threads_to_use = 500
let thread_slot_wrap_modulus = 255

let write_many_threads writer =
  let trace = Trace.Expert.create ~base_time:None writer in
  let pid = Trace.allocate_pid trace ~name:"myproc" in
  let threads =
    Array.init threads_to_use ~f:(fun _ -> Trace.allocate_thread trace ~pid ~name:"t")
  in
  let event_for i =
    Trace.write_instant
      trace
      ~args:[]
      ~thread:threads.(i)
      ~category:"bar"
      ~name:"foo"
      ~time:Time_ns.Span.zero
  in
  (* First use *)
  event_for 0;
  (* Re-use already interned thread *)
  event_for 0;
  (* Use a bunch of other threads to bust the cache *)
  for i = 1 to threads_to_use - 1 do
    event_for i
  done;
  (* Use thread which was evicted again *)
  event_for 0;
  ()
;;

let write_many_threads_manually writer =
  let trace = Trace.Expert.create ~base_time:None writer in
  let pid = Trace.allocate_pid trace ~name:"myproc" in
  (* It sets all the names up front *)
  let thread_name = Tracing_zero.Writer.intern_string writer "t" in
  for i = 2 to threads_to_use + 1 do
    Tracing_zero.Writer.set_thread_name writer ~pid ~tid:i ~name:thread_name
  done;
  let set_slot ~slot ~tid = Tracing_zero.Writer.set_thread_slot writer ~slot ~pid ~tid in
  (* The wrapper interns the thread before the strings *)
  let thread = set_slot ~slot:0 ~tid:2 in
  let name = Tracing_zero.Writer.intern_string writer "foo" in
  let category = Tracing_zero.Writer.intern_string writer "bar" in
  let event_for thread =
    Tracing_zero.Writer.write_instant
      writer
      ~arg_types:Tracing_zero.Writer.Arg_types.none
      ~thread
      ~category
      ~name
      ~ticks:0
  in
  (* First use *)
  event_for thread;
  (* Re-use already interned thread *)
  event_for thread;
  (* Using a bunch of other threads lazily sets each thread as they're used *)
  for i = 1 to threads_to_use - 1 do
    let thread = set_slot ~slot:(i % thread_slot_wrap_modulus) ~tid:(i + 2) in
    event_for thread
  done;
  (* Using thread which was evicted results in it being set to a new slot *)
  let thread = set_slot ~slot:(threads_to_use % thread_slot_wrap_modulus) ~tid:2 in
  event_for thread;
  ()
;;

let%expect_test "check thread cache eviction" =
  let raw_demo = Trace_test_helpers.trace_to_buf write_many_threads_manually in
  let helper_demo = Trace_test_helpers.trace_to_buf write_many_threads in
  Expect_test_patdiff.print_patdiff_s
    [%sexp (raw_demo : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (helper_demo : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {| |}]
;;
