open! Core

(** Abstraction for a source of buffers to write trace data to.

    At the moment this is used for the ability to both write to a file and to an in-memory
    buffer for tests.

    However I also tried to anticipate some of the structure of how the API would work
    which would be required to write to a shared-memory transport involving a ring or
    double-buffering system. *)
module type Destination = sig
  (** Called before writing up to [ensure_capacity] bytes of data to the destination.
      Any older buffers will no longer be used after calling this function, so it's legal
      for a [Destination] to re-use [Iobuf.t]s to avoid allocating.

      All writers are expected to update the Iobuf's [lo], either manually or using
      [Iobuf.Fill]. Data will only be consumed up to the new [lo]. *)
  val next_buf : ensure_capacity:int -> (read_write, Iobuf.seek) Iobuf.t

  (** We will no longer be writing anything. Resources should be flushed and freed. *)
  val close : unit -> unit
end

module type Arg_writers = sig
  type t
  type string_id

  val string : t -> name:string_id -> string_id -> unit
  val int32 : t -> name:string_id -> int -> unit
  val int63 : t -> name:string_id -> int -> unit
  val int64 : t -> name:string_id -> int64 -> unit
  val pointer : t -> name:string_id -> int64 -> unit
  val float : t -> name:string_id -> float -> unit
end

module Tick_translation = struct
  type t =
    { ticks_per_second : int
    ; base_ticks : int
    ; base_time : Time_ns.t
    }

  let epoch_ns =
    { ticks_per_second = 1_000_000_000; base_ticks = 0; base_time = Time_ns.epoch }
  ;;
end
