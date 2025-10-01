open! Core

module Event_type = struct
  type t =
    | Instant
    | Counter of { id : int }
    | Duration_begin
    | Duration_end
    | Duration_complete of { end_time : Time_ns.Span.t }
    | Async_begin of { async_correlation_id : int }
    | Async_instant of { async_correlation_id : int }
    | Async_end of { async_correlation_id : int }
    | Flow_begin of { flow_correlation_id : int }
    | Flow_step of { flow_correlation_id : int }
    | Flow_end of { flow_correlation_id : int }
  [@@deriving sexp_of, compare ~localize]
end

module Thread = struct
  type t =
    { pid : int
    ; tid : int
    ; mutable process_name : string option
    ; mutable thread_name : string option
    }
  [@@deriving sexp_of, compare ~localize]
end

module Parse_error = struct
  type t =
    | No_more_words
    | Timestamp_too_large
    | Invalid_size_on_record
    | Invalid_string_ref
    | Invalid_thread_ref
    | Invalid_tick_initialization
    | Incomplete_record
  [@@deriving sexp_of]
end

module Warnings = struct
  type t =
    { mutable num_unparsed_records : int
    ; mutable num_unparsed_args : int
    }
  [@@deriving sexp_of]
end
