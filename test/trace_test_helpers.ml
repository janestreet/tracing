open! Core
module Parser = Tracing.Parser

let trace_to_buf ?num_temp_strs f =
  let buf = Iobuf.create ~len:100_000 in
  let destination = Tracing_zero.Destinations.iobuf_destination buf in
  let w = Tracing_zero.Writer.Expert.create ?num_temp_strs ~destination () in
  f w;
  Tracing_zero.Writer.close w;
  buf
;;

let rec print_records_until_error parser =
  let event = Parser.parse_next parser in
  print_s [%sexp (event : (Parser.Record.t, Parser.Parse_error.t) Result.t)];
  if Result.is_ok event then print_records_until_error parser
;;

let rec print_records_until parser =
  let event = Parser.parse_next parser in
  print_s [%sexp (event : (Parser.Record.t, Parser.Parse_error.t) Result.t)];
  match event with
  | Ok _ -> print_records_until parser
  | Error err -> err
;;
