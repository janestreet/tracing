let enable_all () =
  let open Probes_lib in
  Self.update (Selected [ Enable, Regex (pattern "trace__.*") ])
;;

let disable_all () =
  let open Probes_lib in
  Self.update (Selected [ Disable, Regex (pattern "trace__.*") ])
;;

let enable ~category =
  let open Probes_lib in
  Self.update (Selected [ Enable, Regex (pattern ("trace__" ^ category ^ "__.*")) ])
;;

let disable ~category =
  let open Probes_lib in
  Self.update (Selected [ Disable, Regex (pattern ("trace__" ^ category ^ "__.*")) ])
;;
