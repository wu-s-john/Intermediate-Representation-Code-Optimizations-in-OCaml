open Async
open Core

let read_json ~filename : Yojson.Safe.t Deferred.t =
  Reader.with_file filename ~f:(fun file ->
      let%bind contents = Reader.contents file in
      let%map json = Process.run_exn ~prog:"bril2json" ~args:[] ~stdin:contents () in 
      Yojson.Safe.from_string json)

let read_bril ~(filename : string) : (Program.t, Json_repr.Error.t) Deferred.Result.t =
  let%map json = read_json ~filename in
  match Json_repr.Program.of_yojson json with
  | Ok json_repr -> Program.of_json_repr json_repr
  | Error error -> Core.Error (`Json_parse_error  error :> Json_repr.Error.t)

let to_json (program : Program.t) : Yojson.Safe.t =
  Program.to_json_repr program |> Json_repr.Program.to_yojson

let to_human_readable_bril (json : Yojson.Safe.t) : string Deferred.t =
  Process.run_exn ~prog:"bril2txt" ~args:[] ~stdin:(Yojson.Safe.pretty_to_string json) ()

let assert_program (program1, filename1) (program2, filename2) : unit Deferred.t =
  let json1 = to_json program1 in
  let json2 = to_json program2 in
  if Yojson.Safe.equal json1 json2 then Deferred.unit
  else
    let%map (bril_program1, bril_program2) =
      Deferred.both (to_human_readable_bril json1) (to_human_readable_bril json2)
    in
    failwithf
      !"Two programs are not equal\n%s:\n%s\n%s:\n%s"
      filename1
      bril_program1
      filename2
      bril_program2
      ()

let test_optimization ~(filename : string) ~(expected : string) ~(f:Program.t -> Program.t) =
  (let open Deferred.Result.Let_syntax in
  let%bind deserialized_program = read_bril ~filename in
  let%map expected_program = read_bril ~filename:expected in
  (f deserialized_program, expected_program))
  |> Deferred.bind ~f:(function
         | Ok (computed_program, expected_program) ->
           assert_program (computed_program, filename) (expected_program, expected)
         | Error error -> failwithf !"Could not deserialize program %{sexp:Json_repr.Error.t}" error ())

let test_local_optimization ~(filename : string) ~(expected : string) ~(f:Program.Block.t -> Program.Block.t) = 
    test_optimization ~filename ~expected ~f:(fun program -> Program.run_local_optimizations program ~f)