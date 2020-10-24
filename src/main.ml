open Opium.Std

let (let*) = Option.bind
let (let+) = Result.bind
let (let>) = Lwt.bind
let (/) = Filename.concat

let (|!) o s =
  match o with
  | Some x -> Ok x
  | None -> Error s

let index = {|
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1.0">
  <script type="text/javascript" defer="defer" src="app.js"></script>
  <title>New gallery</title>
  <style type="text/css">
    body {
      margin:auto;
      max-width:850px;
      line-height:1.6;
      font-size:18px;
      color:#444;
      padding:0 10px;
    }
    h1,h2,h3 { line-height:1.2 }
    img,video { max-width:100% }
    .text-field { width:100% }
  </style>
</head>
<body></body>
</html>
|}

let index =
  get "/" (fun _req ->
      respond' (`Html index)
    )

let app_js =
  get "/app.js" (fun _ -> respond' (`String App_embed.data))

let (|?) o x = match o with Some y -> y | None -> x
let option_get = function
  | Some x -> x
  | None -> failwith "option_get"

type content_item_ty = Text | Image

type part_key =
  | Content_item of int * content_item_ty
  | Title
  | Url_field
  | Content_ids

let store_part_on_disk = function
  | Content_item (_, Image) -> true
  | _ -> false

let part_id = function
  | Content_item (id, _) -> Ok id
  | _ -> Error "part_id: wrong part"

let part_key_of_string s =
  match s with
  | "gal-url" -> Ok Url_field
  | "gal-title" -> Ok Title
  | "gal-content-ids" -> Ok Content_ids
  | _ ->
     try Scanf.sscanf s "gal-img-%d" (fun x -> Ok (Content_item (x, Image)))
     with Scanf.Scan_failure _ ->
       try Scanf.sscanf s "gal-txt-%d" (fun x -> Ok (Content_item (x, Text)))
       with Scanf.Scan_failure _ -> Error ("unknown part key: " ^ s)

let get_header_key header =
  let open Multipart_form in
  let+ v = Header.content_disposition header |! "multipart header: content disposition not found" in
  let+ n = Content_disposition.name v |! "multipart header: name not found" in
  let+ k = part_key_of_string n in
  let fn = Content_disposition.filename v in
  Ok (k, fn)

let validate_url_field s =
  let module Ascii = Astring.Char.Ascii in
  let char_ok c = Ascii.is_alphanum c || List.mem c ['_'; '-'] in
  if Astring.String.for_all char_ok s && s <> "" then Ok s
  else Error "Invalid url field"

let part_filename ~base_dir dirname filename id =
  base_dir / dirname / (string_of_int id ^ Filename.extension filename)

let page_filename ~base_dir dirname =
  base_dir / dirname / "index.html"

let open_out_res file =
  try Ok (open_out_gen [Open_creat; Open_binary; Open_wronly] 0o600 file)
  with Sys_error s -> Error ("open_out: " ^ s)

let mkdir_dirname file =
  let open Bos.OS in
  let d = Filename.dirname file |> Fpath.v in
  Dir.create d |> Result.map_error (fun (`Msg msg) -> msg)

let extract_parts ~base_dir content_type body =
  let module A = Angstrom.Buffered in
  let on_disk = Hashtbl.create 10 in
  let in_mem = Hashtbl.create 10 in
  let errors = ref [] in

  let url_field =
    let memo = ref (Error "XXX") in
    fun () ->
    match !memo with
    | Ok s -> Ok s
    | Error _ ->
       let+ b = Hashtbl.find_opt in_mem Url_field |! "url-field not found while extracting parts" in
       memo := validate_url_field (Buffer.contents b);
       !memo
  in

  let push_to_buffer (b: Buffer.t) = function
    | Some s -> Buffer.add_string b s
    | None -> ()
  in
  let push_to_file cout = function
    | Some s -> output_string cout s
    | None -> ()
  in

  let emitters header =
    begin
      let+ (k, filename) = get_header_key header in
      if store_part_on_disk k then begin
          let+ uf = url_field () in
          let+ id = part_id k in
          let+ filename = filename |! "No filename found for part to store to disk" in
          let filename = part_filename ~base_dir uf filename id in
          let+ (_:bool) = mkdir_dirname filename in
          let+ cout = open_out_res filename in
          Hashtbl.add on_disk k (filename, cout);
          Ok (push_to_file cout, Some k)
        end else begin
          let b = Buffer.create 80 in
          Hashtbl.add in_mem k b;
          Ok (push_to_buffer b, Some k)
        end
    end
    |> function
      | Ok (push, k) -> push, k
      | Error msg -> errors := msg :: !errors; (fun _ -> ()), None
  in

  let parser = Multipart_form.parser ~emitters content_type in
  let state = ref (A.parse parser) in
  let stream = Cohttp_lwt.Body.to_stream body in

  let rec feed () =
    match !state with
    | A.Partial step ->
       let> chunk = Lwt_stream.get stream in
       begin match chunk with
       | Some s -> state := step (`String s)
       | None -> state := step `Eof
       end; feed ()
    | A.Done (_, x) -> Lwt.return x
    | A.Fail _ -> Lwt.fail (Failure "bad multipart format")
  in

  let> _ = feed () in
  let in_mem_parts =
    Hashtbl.fold (fun k b l -> (k, Buffer.contents b) :: l) in_mem [] in
  let on_disk_parts =
    Hashtbl.fold (fun k (fn, cout) l -> close_out cout; (k, fn) :: l) on_disk [] in

  if !errors <> [] then
    Lwt.return (Error (string_of_int (List.length !errors) ^ " deferred errors:\n" ^ (String.concat "\n" !errors)))
  else
    Lwt.return @@
      let+ url_field = List.assoc_opt Url_field in_mem_parts |! "url-field not found" in
      let+ content_ids = List.assoc_opt Content_ids in_mem_parts |! "content-ids not found" in
      let title = List.assoc_opt Title in_mem_parts in
      let in_mem_items =
        List.filter_map (function
            | (Content_item (x, ty), s) -> Some (x, (ty, s))
            | _ -> None
          ) in_mem_parts
      in
      let on_disk_items =
        List.filter_map (function
            | (Content_item (x, ty), fn) -> Some (x, (ty, fn))
            | _ -> None
          ) on_disk_parts
      in
      Ok (url_field, content_ids, title, in_mem_items, on_disk_items)

let get_content_type headers =
  let* ct =
    Cohttp.Header.to_list headers
    |> List.map (fun (k,v) -> String.lowercase_ascii k, v)
    |> List.assoc_opt "content-type"
  in
  Multipart_form.Content_type.of_string (ct ^ "\r\n") |> Result.to_option

let parse_content_ids s =
  try
    String.split_on_char ',' s
    |> List.map int_of_string
    |> Result.ok
  with Failure _ -> Error "invalid content-ids"

let mk_gal_page title content =
  let page_title = match title with
    | Some s -> s
    | None -> "Gallery"
  in
{|
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>|} ^ page_title ^ {|</title>
    <style type="text/css">
      body {
	  margin:auto;
	  max-width:850px;
	  line-height:1.6;
	  font-size:18px;
	  color:#444;
	  padding:0 10px;
      }
      h1,h2,h3 { line-height:1.2 }
      img,video { max-width:100% }
    </style>
  </head>
  <body>
 |} ^
  (match title with
   | Some s -> Printf.sprintf {|<header><h1>%s</h1></header>|} s
   | None -> "")
  ^ "\n\n" ^
    begin
      List.map (function
          | `Text s -> Printf.sprintf {|<p>%s</p>|} s
          | `Img fn -> Printf.sprintf {|<img src="%s">|} fn
        ) content
      |> String.concat "\n\n"
    end
  ^
  {|</body>
</html>|}

let collect_content content_ids in_mem on_disk =
  let rec list_is_success = function
    | (Ok x) :: xs ->
       let+ xs' = list_is_success xs in
       Ok (x :: xs')
    | Error e :: _ ->
       Error e
    | [] -> Ok []
  in

  List.map (fun id ->
      match List.assoc_opt id in_mem with
      | Some (Text, s) -> Ok (`Text s)
      | Some _ -> assert false
      | None ->
         match List.assoc_opt id on_disk with
         | Some (Image, fn) -> Ok (`Img (Filename.basename fn))
         | Some _ -> assert false
         | None ->
            Error ("id " ^ string_of_int id ^ " not found")
    ) content_ids
  |> list_is_success

let form_post ~base_dir =
  post "/" (fun req ->
      print_endline "received post";
      match get_content_type (Cohttp.Request.headers req.request) with
      | None -> respond' (`String ("Error: missing content type"))
      | Some content_type ->
         let> parts = extract_parts ~base_dir content_type req.body in
         begin
           let+ (url_field, content_ids, title, in_mem, on_disk) = parts in
           let+ content_ids = parse_content_ids content_ids in
           let+ content = collect_content content_ids in_mem on_disk in
           let page = mk_gal_page title content in
           let page_fn = page_filename ~base_dir url_field in
           let+ _:bool = mkdir_dirname page_fn in
           let cout = open_out page_fn in
           output_string cout page;
           close_out cout;
           Ok (base_dir / url_field ^ "/")
         end
         |> function
           | Ok uri -> redirect' (Uri.of_string uri)
           | Error msg -> respond' (`String ("Error: " ^ msg))
    )

let static_with_indexes ~local_path ~uri_prefix () =
  let static = Middleware.static ~local_path ~uri_prefix () in
  let filter, name = Rock.Middleware.(filter static, name static) in
  let module String = Opium_kernel__Misc.String in
  let filter handler (req: Request.t) =
    if Request.meth req = `GET then
      let req_path = req |> Request.uri |> Uri.path in
      let maybe_dir () = local_path ^ String.chop_prefix ~prefix:uri_prefix req_path in
      if (req_path |> String.is_prefix ~prefix:uri_prefix)
         && Bos.OS.Dir.exists (Fpath.(v @@ maybe_dir ())) = Ok true
      then
        if Astring.String.is_suffix ~affix:"/" req_path then
          filter handler
            { req with request =
              Cohttp.Request.{ req.request with resource = req_path ^ "index.html" } }
        else
          redirect' (Uri.of_string (req_path ^ "/"))
      else filter handler req
    else filter handler req
  in
  Rock.Middleware.create ~filter ~name

let static base_dir =
  static_with_indexes ~local_path:base_dir ~uri_prefix:("/" ^ base_dir) ()

module Auth = struct
  open Opium.Std

  type user = { username: string }
  let sexp_of_user { username } = Sexplib.Conv.sexp_of_string username
  let user_of_sexp = Sexplib.Conv.string_of_sexp

  module Env = struct
    (* or use type nonrec *)
    type user' = user

    let key : user' Opium.Hmap.key =
      Opium.Hmap.Key.create ("user", sexp_of_user)
  end

  let m auth =
    let filter handler req =
      let need_auth () =
        let headers =
          Cohttp.Header.of_list
            ["WWW-Authenticate", {|Basic realm="Admin password", charset="UTF-8"|}]
        in
        respond' ~code:`Unauthorized ~headers (`String "")
      in
      match req |> Request.headers |> Cohttp.Header.get_authorization with
      | None -> need_auth ()
      | Some (`Other _) ->
         (* handle other, non-basic authentication mechanisms *)
         handler req
      | Some (`Basic (username, password)) -> (
        match auth ~username ~password with
        | None -> need_auth ()
        | Some user ->
           (* we have a user. let's add him to req *)
           let env = Opium.Hmap.add Env.key user (Request.env req) in
           let req = {req with Request.env} in
           handler req )
    in
    Rock.Middleware.create ~name:"http basic auth" ~filter

  let user req = Opium.Hmap.find Env.key (Request.env req)
end

let auth admin_password ~username ~password =
  if username = "admin" && password = admin_password then
    Some Auth.{ username = "admin" }
  else
    None

(* Cursed *)
type opium_app = { port: int; ssl : int option; debug: bool; verbose: bool; routes: int list;
                   middlewares: int list; name: string; not_found: int }
let enable_debug app =
  let app : opium_app = Obj.magic app in
  (Obj.magic { app with debug = true } : App.t)

let gal debug admin_password base_dir port =
  Printf.printf "Using content directory: %s\n%!" base_dir;
  if debug then Printf.printf "Debug enabled\n%!";
  let auth =
    match admin_password with
    | None -> fun x -> x
    | Some pass ->
       Printf.printf "Using authentication\n%!";
       middleware (Auth.m (auth pass))
  in
  let app =
    App.empty
    |> App.cmd_name "gal"
    |> App.port port
    |> index
    |> form_post ~base_dir
    |> app_js
    (* Middlewares. The order matters: it is important that the middleware for
       static content goes before the one for authentication (we only want
       authentication for non-static pages). *)
    |> (if debug then enable_debug else fun x -> x)
    |> middleware (static base_dir)
    |> auth
    |> App.start
  in
  Printf.printf "Listening on port %d...\n%!" port;
  Lwt_main.run app

(* Command line interface *)

open Cmdliner

let port =
  let doc = "Port to listen to" in
  Arg.(value & opt int 3000 & info ~doc ~docv:"PORT" ["port"; "p"])

let base_dir =
  let doc = "Directory where to store the generated content" in
  Arg.(required & pos 0 (some dir) None & info ~doc ~docv:"CONTENT_DIR" [])

let debug =
  let doc = "Enable debugging" in
  Arg.(value & flag & info ~doc ~docv:"DEBUG" ["debug"])

let admin_pass =
  let doc = "Authentication password. The username is \"admin\".
             If this option is not provided, then there is no authentication."
  in
  Arg.(value & opt (some string) None & info ~doc ~docv:"PASSWORD" ["password"])

let cmd =
  let doc = "A simple gallery creation webapp" in
  let man = [
    ]
  in
  Term.(const gal $ debug $ admin_pass $ base_dir $ port),
  Term.info "gal" ~doc ~man ~exits:Term.default_exits

let () = Term.(exit @@ eval cmd)
