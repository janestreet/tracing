(** Helpers for managing tracing probe state inside your executable. *)

(** Turn on all tracing probes. *)
val enable_all : unit -> unit

(** Turn off all tracing probes. *)
val disable_all : unit -> unit

(** Turn on probes within a specific category. *)
val enable : category:string -> unit

(** Turn off probes within a specific category. *)
val disable : category:string -> unit
