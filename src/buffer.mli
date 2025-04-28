open Core
open Tracing_zero

type t

(** Create a trace data buffer holding up to 2*2^size_bits bytes.

    The returned buffer should only operate on data produced by writers with equivalent
    [num_temp_strs]. This option should typically not be set, and defaults to 100. *)
val create : ?num_temp_strs:int -> size_bits:int -> unit -> t

(** Consumes the next portion of tracing data from an iobuf, updating the iobuf. The data
    contained in the iobuf must have the same format as written by a [Writer.t] with the
    same [num_temp_strs] as the buffer. Events that fail to parse/validate are dropped.

    To avoid state corruptions (e.g. referring to unknown interned strings), all data for
    a complete trace should be streamed through the buffer, despite only retaining the
    last N events.

    If the buffer ends with a partial event, only data until (and not including) the final
    partial event's header is consumed from the iobuf. *)
val consume : t -> (read, Iobuf.seek) Iobuf.t -> unit

(** Equivalent to [consume], but if any events fail to parse/validate, returns an error
    status indicating how many events were skipped for what reasons. *)
val try_consume : t -> (read, Iobuf.seek) Iobuf.t -> unit Or_error.t

(** Write out a valid trace including all currently buffered trace events. Creating a
    valid trace also requires reproducing state (e.g. string interning) referred to by the
    events in the buffer.

    The writer must have the same [num_temp_strs] that this buffer was created with. *)
val output : t -> Writer.t -> unit

(** Clears the underlying buffer and resizes it to hold up to 2*2^size_bits bytes. *)
val resize : t -> size_bits:int -> unit
