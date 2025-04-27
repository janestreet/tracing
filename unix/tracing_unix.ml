include Tracing

module Trace = struct
  include Trace

  let create_for_file ~base_time ~filename =
    let writer = Tracing_destinations_unix.file_writer ~filename () in
    Expert.create ~base_time writer
  ;;
end
