open! Core
module Unix = Core_unix
module TW = Tracing_zero.Writer

let%expect_test "demo trace" =
  let buf = Trace_test_helpers.trace_to_buf Tracing_demo.write_demo_trace in
  print_s [%sexp (buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
  (* This is primarily for being notified of accidental changes, and for verifying that
     the added bytes look correct when something is added to the demo trace. To actually
     check that the trace is valid there is a "generate demo" command which uses the same
     function to produce a trace file which can be loaded in Perfetto. *)
  [%expect
    {|
      ("00000000  10 00 04 46 78 54 16 00  30 00 01 00 00 00 c0 00  |...FxT..0.......|"
       "00000010  6a 61 6e 65 5f 74 72 61  63 69 6e 67 00 00 00 00  |jane_tracing....|"
       "00000020  10 00 02 00 00 00 00 00  22 00 01 00 07 00 00 00  |........\".......|"
       "00000030  70 72 6f 63 65 73 73 00  41 00 00 00 00 00 00 00  |process.A.......|"
       "00000040  00 ca 9a 3b 00 00 00 00  00 00 00 00 00 00 00 00  |...;............|"
       "00000050  01 00 00 00 00 00 00 00  22 00 77 00 06 00 00 00  |........\".w.....|"
       "00000060  6d 79 70 72 6f 63 00 00  27 00 01 77 00 00 00 00  |myproc..'..w....|"
       "00000070  01 00 00 00 00 00 00 00  22 00 78 00 08 00 00 00  |........\".x.....|"
       "00000080  6d 79 74 68 72 65 61 64  47 00 02 78 00 01 00 00  |mythreadG..x....|"
       "00000090  02 00 00 00 00 00 00 00  28 00 01 00 00 00 00 00  |........(.......|"
       "000000a0  01 00 00 00 00 00 00 00  33 00 01 00 00 00 00 00  |........3.......|"
       "000000b0  01 00 00 00 00 00 00 00  02 00 00 00 00 00 00 00  |................|"
       "000000c0  22 00 79 00 05 00 00 00  73 74 75 66 66 00 00 00  |\".y.....stuff...|"
       "000000d0  22 00 7a 00 08 00 00 00  6d 79 5f 66 75 6e 63 74  |\".z.....my_funct|"
       "000000e0  44 00 14 01 79 00 7a 00  10 27 00 00 00 00 00 00  |D...y.z..'......|"
       "000000f0  16 00 7a 00 79 00 00 00  40 4b 4c 00 00 00 00 00  |..z.y...@KL.....|"
       "00000100  22 00 14 00 03 00 00 00  77 6f 77 00 00 00 00 00  |\".......wow.....|"
       "00000110  22 00 15 00 04 00 00 00  63 6f 6f 6c 00 00 00 00  |\".......cool....|"
       "00000120  54 00 24 01 79 00 7a 00  c0 cf 6a 00 00 00 00 00  |T.$.y.z...j.....|"
       "00000130  16 00 7a 00 14 00 00 00  16 00 79 00 15 00 00 00  |..z.......y.....|"
       "00000140  c0 e1 e4 00 00 00 00 00  44 00 15 01 79 00 7a 00  |........D...y.z.|"
       "00000150  c0 f3 5e 01 00 00 00 00  11 00 79 00 02 00 00 00  |..^.......y.....|"
       "00000160  01 00 00 00 00 00 00 00  34 00 08 01 00 00 00 00  |........4.......|"
       "00000170  80 84 1e 00 00 00 00 00  01 00 00 00 00 00 00 00  |................|"
       "00000180  24 00 00 01 79 00 7a 00  40 66 03 01 00 00 00 00  |$...y.z.@f......|"
       "00000190  44 00 15 01 79 00 7a 00  00 36 6e 01 00 00 00 00  |D...y.z..6n.....|"
       "000001a0  11 00 79 00 01 00 00 00  02 00 00 00 00 00 00 00  |..y.............|"
       "000001b0  24 00 02 01 79 00 7a 00  00 2d 31 01 00 00 00 00  |$...y.z..-1.....|"
       "000001c0  34 00 09 01 00 00 00 00  c0 e1 e4 00 00 00 00 00  |4...............|"
       "000001d0  01 00 00 00 00 00 00 00  74 00 33 01 79 00 7a 00  |........t.3.y.z.|"
       "000001e0  00 5a 62 02 00 00 00 00  23 00 7a 00 00 00 00 00  |.Zb.....#.z.....|"
       "000001f0  00 00 00 00 00 00 00 c0  11 00 79 00 ff ff ff ff  |..........y.....|"
       "00000200  25 00 77 00 00 00 00 00  00 00 00 00 00 00 f0 bf  |%.w.............|"
       "00000210  44 00 16 01 79 00 7a 00  40 78 7d 01 00 00 00 00  |D...y.z.@x}.....|"
       "00000220  11 00 79 00 01 00 00 00  02 00 00 00 00 00 00 00  |..y.............|"
       "00000230  44 00 16 01 79 00 7a 00  80 ba 8c 01 00 00 00 00  |D...y.z.........|"
       "00000240  11 00 79 00 02 00 00 00  01 00 00 00 00 00 00 00  |..y.............|"
       "00000250  34 00 0a 01 00 00 00 00  00 2d 31 01 00 00 00 00  |4........-1.....|"
       "00000260  01 00 00 00 00 00 00 00  44 00 11 01 79 00 7a 00  |........D...y.z.|"
       "00000270  c0 cf 6a 00 00 00 00 00  11 00 79 00 01 00 00 00  |..j.......y.....|"
       "00000280  01 00 00 00 00 00 00 00  44 00 17 01 79 00 7a 00  |........D...y.z.|"
       "00000290  40 81 ba 01 00 00 00 00  11 00 79 00 02 00 00 00  |@.........y.....|"
       "000002a0  01 00 00 00 00 00 00 00  44 00 17 01 79 00 7a 00  |........D...y.z.|"
       "000002b0  c0 05 d9 01 00 00 00 00  11 00 79 00 01 00 00 00  |..........y.....|"
       "000002c0  02 00 00 00 00 00 00 00  54 00 11 01 79 00 7a 00  |........T...y.z.|"
       "000002d0  00 2d 31 01 00 00 00 00  25 00 79 00 00 00 00 00  |.-1.....%.y.....|"
       "000002e0  00 00 00 00 00 00 04 40  01 00 00 00 00 00 00 00  |.......@........|"
       "000002f0  54 00 11 01 79 00 7a 00  00 2d 31 01 00 00 00 00  |T...y.z..-1.....|"
       "00000300  23 00 79 00 00 00 00 00  ff ff ff ff ff ff ff 7f  |#.y.............|"
       "00000310  01 00 00 00 00 00 00 00  54 00 11 01 79 00 7a 00  |........T...y.z.|"
       "00000320  00 2d 31 01 00 00 00 00  27 00 79 00 00 00 00 00  |.-1.....'.y.....|"
       "00000330  ff ff ff ff ff ff ff 7f  01 00 00 00 00 00 00 00  |................|") |}]
;;

let test_file_writer ~f =
  (* Writing the demo trace 100 times hopefully lets us test the case of calling
     [next_buf] more than once. *)
  let one_hundred_demos writer =
    for _ = 1 to 100 do
      Tracing_demo.write_demo_trace writer
    done
  in
  let buf = Trace_test_helpers.trace_to_buf one_hundred_demos in
  let demo_trace_size = Iobuf.length buf in
  let filename = Filename_unix.temp_file "test-trace-file-" "" in
  Sys_unix.remove filename;
  let destination = f filename in
  let writer = TW.Expert.create ~destination () in
  one_hundred_demos writer;
  TW.close writer;
  let stat = Unix.stat filename in
  let file_size = Int64.to_int_exn stat.st_size in
  Expect_test_helpers_base.require_equal [%here] (module Int) file_size demo_trace_size;
  Sys_unix.remove filename;
  file_size
;;

let%expect_test "writing directly to a file" =
  let buffer_size = 4096 in
  let file_size =
    test_file_writer ~f:(fun filename ->
      Tracing_destinations_unix.direct_file_destination ~buffer_size ~filename ())
  in
  (* Ensure that we actually hit the buffer flush case multiple times. *)
  assert (file_size > buffer_size * 3);
  [%expect {||}]
;;

let%expect_test "write notification logic works" =
  let logging_destination buf =
    let module Dest = struct
      let next_buf ~ensure_capacity =
        Core.print_s [%message "next_buf" (ensure_capacity : int)];
        assert (ensure_capacity < Iobuf.length buf);
        buf
      ;;

      let close () = Core.print_s [%message "close"]
    end
    in
    (module Dest : TW.Expert.Destination)
  in
  let buf = Iobuf.create ~len:10_000 in
  (* Test the case of providing a buffer where the window doesn't start at the limits.
     Some destinations don't do this.*)
  Iobuf.advance buf 64;
  let destination = logging_destination buf in
  let w = TW.Expert.create ~destination () in
  let thread = TW.set_thread_slot w ~slot:0 ~pid:1 ~tid:2 in
  let str = TW.intern_string w "foo" in
  let write_16_byte_event () =
    TW.write_instant
      w
      ~arg_types:TW.Arg_types.none
      ~thread
      ~category:str
      ~name:str
      ~ticks:0
  in
  TW.Expert.flush w;
  write_16_byte_event ();
  TW.Expert.flush w;
  write_16_byte_event ();
  TW.close w;
  (* Should be the header, then close. *)
  [%expect {|
    (next_buf (ensure_capacity 8))
    close |}]
;;
