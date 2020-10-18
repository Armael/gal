open Bos
let (>>=) = Result.bind

let main () =
  let src, dst =
    match Sys.argv |> Array.to_list |> List.tl with
    | src :: dst :: [] -> src, dst
    | _ ->
      Printf.eprintf "usage: %s <src> <dst>\n" Sys.argv.(0);
      exit 1
  in

  OS.Path.must_exist (Fpath.v src) >>= fun src ->
  OS.File.read src >>= fun src_contents ->
  OS.File.writef (Fpath.v dst) "let data = %S\n" src_contents

let () =
  match main () with
  | Ok () -> ()
  | Error (`Msg msg) ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
