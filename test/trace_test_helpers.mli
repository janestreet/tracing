open! Core

val trace_to_buf
  :  ?num_temp_strs:int
  -> (Tracing_zero.Writer.t -> unit)
  -> (read_write, 'a) Iobuf.t

val print_records_until_error : Tracing.Parser.t -> unit
