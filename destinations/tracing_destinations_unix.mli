open! Core

module type Destination = Tracing_zero.Writer_intf.Destination

(** Write to a file using synchronous writes, not suitable for low latency applications. *)
val direct_file_destination
  :  ?buffer_size:int
  -> filename:string
  -> unit
  -> (module Destination)

(** Write to a file in some way with the best available performance. *)
val file_destination : filename:string -> unit -> (module Destination)

(** Write to a generic file descriptor. *)
val fd_destination
  :  ?buffer_size:int
  -> fd:Core_unix.File_descr.t
  -> unit
  -> (module Destination)

val file_writer : ?num_temp_strs:int -> filename:string -> unit -> Tracing_zero.Writer.t
