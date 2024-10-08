
## Release v0.17.0

- Added support for asynchronous begin/end/instant events to the trace
  writer and parser.

- Added option to preserve interned strings' contents in a hash table.

- Extracted platform-specific tracing destinations into the separate library
  `tracing_destinations_unix`.

- Refactored how the parser handles incomplete event data, fixing a bug.

- Moved full writer signature to `zero/writer_intf.ml`.

## Release v0.16.0

- Added 64-bit integer and pointer event arguments.

- Added buffer type (`src/buffer.mli`) that consumes a real-time stream of tracing data
  and only stores the last N bytes, while preserving global state (e.g. interned strings).

- Changed standard file extension from ".ftf" to ".fxt".

- Moved `Tracing_tool_output` into its own library.

- Updated trace parser:
    - Support parsing an event stream split over multiple data buffers.
    - Detect, report, and potentially resume incomplete event records.

- Updated trace writer:
    - Removed `wrote_bytes` from the destination API: writers are now required to update
      `lo` when writing to buffers returned by `next_buf`.
    - Added unix file descriptor destination.
    - Added various utility functions to `Writer.Expert`.
    - Reserved 17 'dynamic' string interning slots, separate from temp string slots.
      This region is intended to be used via a ppx frontend.
    - Clarified documentation.
