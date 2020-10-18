open Brr
module RList = ReactiveData.RList

module Id : sig
  type t
  val fresh : unit -> t
  val to_string : t -> string
end = struct
  type t = int
  let id = ref 0
  let fresh () = incr id; !id
  let to_string = string_of_int
end

type item =
  | Image of { img_uri : Jstr.t; img_input : El.t; visible: bool }
  | Text of { txt_input : El.t }

let at_hide = At.v (Jstr.of_string "style") (Jstr.of_string "display:none")

let contents, contents_handle = RList.create []
let contents_elts = RList.map (fun (id, item) ->
  id,
  match item with
  | Image { img_uri; img_input; visible } ->
    El.div [
      El.img ~at:([At.v (Jstr.of_string "src") img_uri] @
                  if not visible then [at_hide] else [])
        ();
      img_input
    ]
  | Text { txt_input } ->
    txt_input
) contents

let list_find_with_id id =
  let rec loop i = function
    | [] -> None
    | (id', x) :: xs -> if id = id' then Some (i, x) else loop (i+1) xs
  in
  loop 0

let find_with_id id =
  list_find_with_id id (RList.value contents)

let contents_elts_with_del =
  RList.map (function (id, elt) ->
    let delete_button = El.button [ El.txt' "X" ] in
    let on_delete _ =
      match find_with_id id with
      | Some (i, _) -> RList.remove i contents_handle
      | None -> Console.(log [str "could not find index for id: "; id])
    in
    Ev.listen Ev.click on_delete (El.as_target delete_button);
    El.p [elt; delete_button]
  ) contents_elts

let show_image id file =
  match find_with_id id with
  | Some (idx, Image { img_uri = _; img_input; visible = _ }) ->
    let blob = File.as_blob file in
    let uri = Blob.data_uri blob in
    Fut.await uri (function
      | Ok uri ->
        RList.update (id, Image { img_uri = uri; img_input; visible = true })
          idx contents_handle
      | Error e ->
        Console.(log [Jv.Error.message e])
    )
  | _ ->
    ()

let add_image () =
  let img_id = Id.fresh () in
  let i = El.input ~at:At.[
    type' (Jstr.v "file");
    name (Jstr.v ("gal-img-" ^ Id.to_string img_id))
  ] () in
  El.set_inline_style El.Style.display (Jstr.v "none") i;
  Ev.listen Ev.change (fun _ -> show_image img_id (El.Input.files i |> List.hd))
    (El.as_target i);
  El.click i;
  RList.snoc (img_id, Image { img_uri = Jstr.v "#"; img_input = i; visible = false })
    contents_handle;
  ()

let add_image_button () =
  let b = El.button [ El.txt' "Add image…" ] in
  Ev.listen Ev.click (fun _ -> add_image ()) (El.as_target b);
  b

let txt_input ~id ?(at = []) ?(cl = "text-field") () =
  let txt_id = id in
  El.input ~at:(At.[type' (Jstr.v "text");
                    name (Jstr.v txt_id);
                    class' (Jstr.v cl);
                    v (Jstr.v "autocomplete") (Jstr.v "off");
                   ] @ at)
    ()

let add_text () =
  let txt_id = Id.fresh () in
  let i = txt_input ~id:("gal-txt-" ^ Id.to_string txt_id) () in
  RList.snoc (txt_id, Text { txt_input = i }) contents_handle

let add_text_button () =
  let b = El.button [ El.txt' "Add text…" ] in
  Ev.listen Ev.click (fun _ -> add_text ()) (El.as_target b);
  b

let content_ids l =
  List.map fst l
  |> List.map Id.to_string
  |> String.concat ","

let main () =
  let gal_url =
    El.div [
      El.label ~at:At.[for' (Jstr.v "gal-url-field")] [El.txt' "Gallery url"];
      txt_input ~id:"gal-url" ~cl:"gal-url-field" ~at:At.[true' (Jstr.v "required")] ();
    ]
  in
  let title = txt_input ~id:"gal-title" ~at:At.[placeholder (Jstr.v "Gallery title…")] () in
  let viewer = El.div [] in
  let content_ids_field = txt_input ~id:"gal-content-ids" ~at:[at_hide] () in
  let submit = El.input ~at:At.[
    type' (Jstr.v "submit");
    v (Jstr.v "value") (Jstr.v "Submit");
    at_hide
  ] () in
  let submit_button = El.button [ El.txt' "Submit" ] in
  Ev.listen Ev.click (fun _ -> El.click submit) (El.as_target submit_button);
  let form = El.form ~at:At.[
    v (Jstr.v "method") (Jstr.v "post");
    v (Jstr.v "enctype") (Jstr.v "multipart/form-data");
  ] [content_ids_field;
     gal_url;
     title;
     viewer;
     submit] in
  let handle_contents =
    React.S.map (El.set_children viewer) (RList.signal contents_elts_with_del) in
  let handle_content_ids =
    React.S.map (fun l -> El.set_prop El.Prop.value (Jstr.v (content_ids l)) content_ids_field)
      (RList.signal contents)
  in
  El.set_children (Document.body G.document) [
    form;
    El.p [add_image_button (); add_text_button ()];
    El.p [submit_button]
  ];
  [handle_contents; handle_content_ids]

let _h = main ()
