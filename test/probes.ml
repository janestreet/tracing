open! Core
module Writer = Tracing_zero.Writer

let tick_translation = Tracing_probes.For_testing.tick_translation
let start_args = Writer.Arg_types.create ~int32s:1 ()

let write_with_probes ~n_before ~n_after =
  let buf_until = Destinations.Buffer_until_initialized.create () in
  let writer =
    Writer.Expert.create
      ~destination:(Destinations.Buffer_until_initialized.to_destination buf_until)
      ()
  in
  Writer.write_tick_initialization writer tick_translation;
  let _thread = Writer.set_thread_slot writer ~slot:0 ~pid:1 ~tid:2 in
  let probe_begin, probe_end =
    Tracing_probes.Event.For_testing.create_span_for
      ~writer
      ~async:false
      ~arg_types:start_args
      ~category:"test"
      ~name:"foo"
  in
  let arg_name = Writer.intern_string writer "arg" in
  let write_probe_events n =
    for _ = 1 to n do
      Tracing_probes.Event.write_to writer probe_begin;
      Writer.Expert.Write_arg_unchecked.int32 writer ~name:arg_name 123;
      Tracing_probes.Event.write_to writer probe_end
    done
  in
  (* These events get written into a temporary buffer and should be copied over once the
     destination is set. *)
  write_probe_events n_before;
  let buf = Iobuf.create ~len:100_000 in
  Destinations.Buffer_until_initialized.set_destination
    buf_until
    (Tracing_zero.Destinations.iobuf_destination buf);
  write_probe_events n_after;
  Writer.close writer;
  (* The probes use real rdtsc for ticks and allowing that to be mocked would degrade
     performance, so instead we go back over the buffer and zero out the places we think
     should be timestamps. *)
  let len = Iobuf.length buf in
  let n = n_before + n_after in
  (* a 3-word start event and a 2-word end event *)
  let size_per_iter = (8 * 3) + (8 * 2) in
  for i = 0 to n - 1 do
    (* the argument comes last in the start event, so there's 3 words after the time *)
    Iobuf.memset buf ~pos:(len - (i * size_per_iter) - (8 * 4)) ~len:8 Char.min_value;
    Iobuf.memset buf ~pos:(len - (i * size_per_iter) - 8) ~len:8 Char.min_value
  done;
  buf
;;

let write_with_reference_impl ~n =
  let buf = Iobuf.create ~len:100_000 in
  let destination = Tracing_zero.Destinations.iobuf_destination buf in
  let w = Writer.Expert.create ~destination () in
  Writer.write_tick_initialization w Tracing_probes.For_testing.tick_translation;
  let thread = Writer.set_thread_slot w ~slot:0 ~pid:1 ~tid:2 in
  let category = Writer.intern_string w "test" in
  let name = Writer.intern_string w "foo" in
  let arg_name = Writer.intern_string w "arg" in
  for _ = 1 to n do
    Writer.write_duration_begin w ~arg_types:start_args ~thread ~category ~name ~ticks:0;
    Writer.Write_arg.int32 w ~name:arg_name 123;
    Writer.write_duration_end
      w
      ~arg_types:Writer.Arg_types.none
      ~thread
      ~category
      ~name
      ~ticks:0
  done;
  Writer.close w;
  buf
;;

(* Test that probes are writing data correctly by comparing against a reference
   implementation using the standard writing functions. *)
let%expect_test "test probe equivalency" =
  (* use a big enough number before the destination is set that we write to multiple temp
     buffers and thus exercise the buffer switching case. *)
  let n_before, n_after = 2, 2 in
  let buf = write_with_probes ~n_before ~n_after in
  let buf_ref = write_with_reference_impl ~n:(n_before + n_after) in
  Expect_test_patdiff.print_patdiff_s
    [%sexp (buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
    [%sexp (buf_ref : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  [%expect {||}]
;;
