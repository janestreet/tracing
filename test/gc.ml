open! Core

let%test_module "gc tracing" =
  (module struct
    module Parser = Tracing.Parser

    (* Uses a Set to coalesce [major_gc_slice] events,
       where we shouldn't rely on the count *)
    let rec found_gc_types parser =
      match Parser.parse_next parser with
      | Ok (Event { event_type = Duration_complete _; category; name; _ }) ->
        if String.( = ) "gc" (Parser.lookup_string_exn parser ~index:category)
        then Set.add (found_gc_types parser) (Parser.lookup_string_exn parser ~index:name)
        else found_gc_types parser
      | Error No_more_words -> String.Set.empty
      | Error e -> raise_s [%sexp (e : Parser.Parse_error.t)]
      | Ok _ -> found_gc_types parser
    ;;

    let%expect_test "simple" =
      let buf = Iobuf.create ~len:10_000 in
      Tracing_probes.Expert.set_destination
        (Tracing_zero.Destinations.iobuf_destination buf);
      Tracing_record_gc.record_recent_gc_events ();
      Gc.major ();
      Gc_recent_events.For_testing.write_test_event ();
      Tracing_record_gc.record_recent_gc_events ();
      Tracing_probes.close ();
      let parser = Iobuf.read_only buf |> Tracing.Parser.create in
      let found = found_gc_types parser in
      print_s [%sexp (found : String.Set.t)];
      [%expect {|
        (major_gc_slice minor_gc test_event) |}]
    ;;
  end)
;;
