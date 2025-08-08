open! Core

(** Abstraction for a source of buffers to write trace data to.

    At the moment this is used for the ability to both write to a file and to an in-memory
    buffer for tests.

    However I also tried to anticipate some of the structure of how the API would work
    which would be required to write to a shared-memory transport involving a ring or
    double-buffering system. *)
module type Destination = sig
  (** Called before writing up to [ensure_capacity] bytes of data to the destination. Any
      older buffers will no longer be used after calling this function, so it's legal for
      a [Destination] to re-use [Iobuf.t]s to avoid allocating.

      All writers are expected to update the Iobuf's [lo], either manually or using
      [Iobuf.Fill]. Data will only be consumed up to the new [lo]. *)
  val next_buf : ensure_capacity:int -> (read_write, Iobuf.seek) Iobuf.t

  (** We will no longer be writing anything. Resources should be flushed and freed. *)
  val close : unit -> unit
end

module type Arg_writers = sig
  type t
  type string_id

  val interned_string : t -> name:string_id -> string_id -> unit

  (* [Expert.update_size_for_inline_string] must be called before [inline_string] *)
  val inline_string : t -> name:string_id -> string -> unit
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

(** This is the low-level zero-alloc API for writing Fuchsia Trace Format traces. The API
    pays some performance costs to check some format constraints on the events you write,
    but not too many.

    Most users probably want to use the [Tracing.Trace] higher-level API instead. A large
    part of the purpose of this API was to figure out how to efficiently write FTF traces
    without allocating. The low-overhead probe path uses a bunch of the underlying code
    and approach from this interface although uses the [Expert] submodule to precompute
    event headers. *)
module type S = sig
  type t

  val close : t -> unit

  module Tick_translation = Tick_translation

  val make_tick_translation : unit -> Tick_translation.t
  val write_tick_initialization : t -> Tick_translation.t -> unit

  module String_id : sig
    type t [@@deriving equal ~localize] [@@immediate]

    val empty : t
    val max_number_of_temp_string_slots : int
    val of_int : int -> t
  end

  val max_interned_string_length : int

  (** Intern a string into the trace so that it can be referred to with very low cost.
      Note that this does not check if the string has already been interned, see
      [intern_string_cached].

      Note that only around 32k strings can be interned this way, so use it for things
      like identifiers where there won't be that many. See [set_temp_string_slot] for
      things like string arguments with many possible values.

      See the comment at the top of [lib/tracing/src/trace.mli] for more info on string
      interning. *)
  val intern_string : t -> string -> String_id.t

  (** This interns a string while re-using a set of 100 reserved string IDs (by default,
      the number can be overriden at writer creation). Setting the string in a slot
      overwrites what was previously in that slot so any further events written in the
      trace see the new value. This allows arbitrarily many unique strings to be used in a
      trace, unlike [intern_string]. *)
  val set_temp_string_slot : t -> slot:int -> string -> String_id.t

  val num_temp_strs : t -> int

  (** The trace format interns the 64 bit thread and process IDs into an 8-bit thread ID
      and we expose this to the user. *)
  module Thread_id : sig
    type t [@@immediate]

    val of_int : int -> t
  end

  (** Similar to [set_temp_string_slot], interns a thread into a slot ID, overwriting any
      thread which may have previously been in that slot. The number of thread slots is
      very limited (0<=slot<255) so you may need to manage them carefully.

      If a [pid] is the same as the [tid], Perfetto will consider that thread a "main
      thread" and sort it first among the threads, contrary to its usual alphabetical
      sorting by thread name. So if you don't want this to happen allocate tids such that
      they're never the same as a pid.

      Note that Perfetto doesn't require tids to be unique across different pids, but the
      Fuchsia Trace Format spec implies they should be. I think it's safe to assume that
      any tool Jane Street uses will allow per-process tids but it's still safer to make
      them globally unique. *)
  val set_thread_slot : t -> slot:int -> pid:int -> tid:int -> Thread_id.t

  (** Sets the name on the collapsible process section header in the UI.

      Perfetto sorts these headers by pid. *)
  val set_process_name : t -> pid:int -> name:String_id.t -> unit

  (** Sets the name on a thread track.

      Perfetto sorts threads within a process alphabetically. *)
  val set_thread_name : t -> pid:int -> tid:int -> name:String_id.t -> unit

  (** Events are written with a header which specifies how large the record is and how
      many arguments it has, which means you need to pre-commit to how many arguments of
      each type you will later write for an event. This is checked and will throw an
      exception if you write another event or close the writer without having written the
      correct arguments. *)
  module Arg_types : sig
    type t

    (** Use [none] if you aren't going to write any arguments for an event *)
    val none : t

    (** If you're going to write arguments, provide the count of each type you'll write. *)
    val create
      :  ?int64s:int
      -> ?int32s:int
      -> ?floats:int
      -> ?interned_strings:int
      -> ?inlined_strings:int
      -> unit
      -> t
  end

  (** Most event writer functions take a common set of arguments including a commitment to
      what event arguments will be added ([arg_types]), a thread the event occured on, a
      [category] which is an arbitrary string classifying the event visible in UIs and
      potentially used for filtering, a [name] that's the main label for the event, and a
      timestamp in "ticks" which defaults to nanoseconds since the start of the trace, but
      the format allows adjusting to other units like rdtsc clock cycles. *)
  type 'a event_writer =
    t
    -> arg_types:Arg_types.t
    -> thread:Thread_id.t
    -> category:String_id.t
    -> name:String_id.t
    -> ticks:int
    -> 'a

  (** An event with a time but no duration. *)
  val write_instant : unit event_writer

  (** A counters event uses its arguments to specify "counters" which may be represented
      by trace viewers as a chart over time. Its arguments must be numerical and there
      should be at least one.

      The counter ID is in theory for associating events that should be plotted on the
      same graph but in practice Perfetto ignores it and uses the name. The
      [Tracing.Trace] wrapper chooses an ID based on the name to match this. *)
  val write_counter : (counter_id:int -> unit) event_writer

  (** Begin a duration slice which will be finished with a matching end event. *)
  val write_duration_begin : unit event_writer

  (** End a duration event, should be properly nested and with matching name/category *)
  val write_duration_end : unit event_writer

  (** A duration event where the start and end are known up front.

      Takes 3*8 bytes instead of 2*2*8 bytes for separate events, saving 8 bytes per span *)
  val write_duration_complete : (ticks_end:int -> unit) event_writer

  (** Begin an async slice. [async_id] disambiguates concurrent contexts. *)
  val write_async_begin : (async_id:int -> unit) event_writer

  (** Write an event with a time but no duration associated with an async context. *)
  val write_async_instant : (async_id:int -> unit) event_writer

  (** End an async slice. *)
  val write_async_end : (async_id:int -> unit) event_writer

  (** Flow events connect enclosing duration events with arrows in the trace viewer.

      See [Tracing.Flow.t] to make it easier to write the correct flow event type. *)

  (** Begins a flow, the chronologically first event in each flow must use this event.

      Multiple flows with different IDs can start from one enclosing duration slice. *)
  val write_flow_begin : t -> thread:Thread_id.t -> ticks:int -> flow_id:int -> unit

  (** An intermediate step in the flow that's neither the first or last step. *)
  val write_flow_step : t -> thread:Thread_id.t -> ticks:int -> flow_id:int -> unit

  (** Close a flow with a final step. Perfetto allows the flow_id to be re-used after. *)
  val write_flow_end : t -> thread:Thread_id.t -> ticks:int -> flow_id:int -> unit

  (** These argument writers need to be called immediately after an event writer with
      matching [Arg_types] counts for each type. *)
  module Write_arg : Arg_writers with type t := t and type string_id := String_id.t

  module Expert : sig
    module type Destination = Destination

    val create : ?num_temp_strs:int -> destination:(module Destination) -> unit -> t

    (** Creates writer without writing a FXT file header to the destination. *)
    val create_no_header
      :  ?num_temp_strs:int
      -> destination:(module Destination)
      -> unit
      -> t

    (** Switches destination for future data. Use with care; does not reset any state. *)
    val set_destination : t -> destination:(module Destination) -> unit

    (** Writes raw byte stream to the current destination. *)
    val write_bytes : t -> bytes:Bytes.t -> unit

    (** Writes raw byte stream to the current destination. *)
    val write_iobuf : t -> buf:(read, Iobuf.seek) Iobuf.t -> unit

    (** Interns a string directly to the specified slot (whereas [set_temp_string_slot]
        may shift the index since certain indices are reserved for internal use). Will
        raise when setting slot 1 to any string other than "process".

        Useful for preserving the string ID usage of parsed traces in a way that can lead
        to exact byte equality after a round trip through parsing and writing. *)
    val set_string_slot : t -> slot:int -> string -> String_id.t

    (** Immediately ask the destination for a new buffer even if the current one isn't
        full. This is intended for use by the probe infrastructure when a destination for
        the global writer is initialized. *)
    val force_switch_buffers : t -> unit

    (** Finish all pending writes to underlying buffer.

        This is currently only intended for use by tests. *)
    val flush : t -> unit

    type header

    val set_name : header:header -> name:String_id.t -> header
    val update_size_for_name : header:header -> name:string -> header
    val update_size_for_inline_string : header:header -> inline_string:string -> header

    (** For use with [precompute_header_and_size] *)
    module Event_type : sig
      type t

      val instant : t
      val counter : t
      val duration_begin : t
      val duration_end : t
      val duration_complete : t
      val async_begin : t
      val async_end : t
      val async_instant : t
      val flow_begin : t
      val flow_step : t
      val flow_end : t
    end

    (** Pre-compose an event header word, for use with [write_from_header_with_tsc] *)
    val precompute_header
      :  event_type:Event_type.t
      -> extra_words:int
      -> arg_types:Arg_types.t
      -> thread:Thread_id.t
      -> category:String_id.t
      -> name:String_id.t
      -> header

    (** Interns a string into one of 17 temporary slots reserved for ppx_tracing. These
        slots are exclusively used to store the category, name, and names of arguments
        when they cannot be interned globally. Unlike [set_temp_string_slot], strings
        interned to these slots are valid *only* immediately after interning, as the slot
        will be reused in subsequent probes. *)
    val set_dyn_slot : t -> slot:int -> string -> String_id.t

    (** Write an event using a pre-composed header, and using less safety checking than
        the normal event writing functions, intended for low-overhead probe
        instrumentation. Also uses the [rdtsc] counter for the ticks field, meaning this
        [Writer] must have been created with a calibrated [ticks_per_second] field to have
        correct timing.

        The pre-composition itself saves about 3ns per event. The omission of unnecessary
        checks saves additional time. Overall in benchmarks this is about 2x faster than
        using the usual [write_*] functions, saving about 6ns per event.

        The only caller-visible check omission should be that it doesn't check the
        corectness of arguments, which can result in invalid trace files if the arguments
        written don't match the header. Users of this function must use
        [Write_arg_unchecked] because it doesn't set the necessary state for checking. *)
    val write_from_header_with_tsc : t -> header:header -> unit

    (** Same as [write_from_header_with_tsc] but but returns ticks. See
        [Tracing.Writer.write_duration_instant]. *)
    val write_from_header_and_get_tsc : t -> header:header -> Time_stamp_counter.t

    (** Unchecked writing of the result of [write_from_header_and_get_tsc] after the
        arguments. *)
    val write_tsc : t -> Time_stamp_counter.t -> unit

    (** Unchecked write of a string stream. *)
    val write_string_stream : t -> string -> unit

    (** Unchecked write of a correlation id to complete an async event. *)
    val write_async_id : t -> int -> unit

    (** Enables or disables mapping [String_id.t] to [string] when interning strings.

        Use with caution: Enabling this will allocate upon interning strings and cost an
        additional [Hashtbl.add] call. *)
    val set_string_map_allocate_on_intern : t -> enable:bool -> unit

    (** Returns a string represented by the corresponding [String_id.t] *)
    val string_of_string_id : t -> String_id.t -> string option

    (** Unchecked versions of the [Write_arg] functions that can result in invalid traces. *)
    module Write_arg_unchecked :
      Arg_writers with type t := t and type string_id := String_id.t
  end
end
