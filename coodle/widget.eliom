[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Eliom_service
]

(* [user_pwd_form service btxt] is a html form that asks for username and
 * password, button having [btxt] and it will post through [service]*)
let user_pwd_form service btxt =
  div[Form.post_form ~service
        (fun (name1, name2) ->
           [p [
              pcdata "Username: ";
              Form.input ~input_type:`Text ~name:name1 ~value:""
                Form.string;
              br ();
              br ();
              pcdata "Password: ";
              Form.input ~input_type:`Password ~name:name2 ~value:""
                Form.string;
              br ();
              Form.input ~input_type:`Submit ~value:btxt
                Form.string
            ]]) ();
     ]


(* [create_event_form] is the html form to create a event and it will post
 * through [service] *)
let create_event_form service =
  div[Form.post_form ~service
        (fun (name,(time,(desc,anon))) ->
           [p [
              pcdata "Name of Event: ";
              Form.input ~input_type:`Text ~name:name ~value:""
              Form.string;
              br ();
              br ();
              pcdata "Start time: ";
              Form.input ~input_type:`Datetime_local ~name:time ~value:""
              Form.string;
              br ();
              br ();
              pcdata "Anonymous Selection: ";
              Form.input ~input_type: `Checkbox ~name:anon Form.bool;
              br ();
              br ();
              pcdata "Event Description: ";
              br ();
              Form.textarea ~name:desc ~value:"N/A"();
              br ();
              Form.input ~input_type:`Submit ~value:"Create"
                Form.string
            ]]) ();
     ]

(* [setting_form service btn] asks for username, button having [btn] as text,
 * and post through [service] *)
let setting_form service btn =
  div[Form.post_form ~service
        (fun (name1) ->
           [p [
              pcdata "Username: ";
              Form.input ~input_type:`Text ~name:name1 ~value:""
                Form.string;
              Form.input ~input_type:`Submit ~value: btn
                Form.string
            ]]) ();]

(* [finalize_form service]
 * and post through [service] *)
let finalize_form service =
  div[Form.post_form ~service
        (fun () ->
           [p [
              Form.input ~input_type:`Submit ~value:"Finalize"
                Form.string
            ]]) ();
     ]

(* [quit_form service] asks for username, button having [btn] as text,
 * and post through [service] *)
let quit_form service =
  div[Form.post_form ~service
        (fun () ->
           [p [
              Form.input ~input_type:`Submit ~value:"Quit"
                Form.string
            ]]) ();
     ]

(* [inspection_table] is an empty table at the start of instantiation.
 * client side handler will dynamically add and remove user name into it
 * when mouse hover over a time slot*)
let inspection_table event_id user_id =
  let access = Database.user_event_right event_id user_id in
  let classlist =
    match access with
    | Admin
    | Manager -> ["inspection-table"]
    | _ -> ["unvisible"; "inspection-table"]
  in div [div ~a:[a_id "inspect-area"]
     [   div ~a: [a_id "numberpar"]
          [ p ~a: [a_class classlist] [pcdata "Number of Participants: "];
            table ~a:[a_id "num"] []];
        div ~a: [a_id "part"]
          [ p ~a: [a_class classlist] [pcdata "Participants: "];
            table ~a:[a_id "inspect"][] ]];
          div ~a:[a_id "txt-area"][]]

(* server side cache for the event page*)
let%server event_cache : (string, (int * int) list) Eliom_cscache.t =
  Eliom_cscache.create ()

(* server side function to get event cache *)
let%server get_cache (cache_type : string) = Lwt.return ([] : (int * int) list)

let%server get_cache_rpc =
  Eliom_client.server_function [%derive.json: string] (fun s -> get_cache s)

(* client side function to get even cache*)
let%client get_cache = ~%get_cache_rpc

(* [get_attribute attr dom] is the content of [dom]'s attribute [attr] *)
let%client get_attribute attr dom =
    Js.Opt.case
      ((dom##getAttribute (Js.string attr)))
      (fun () -> 0) (fun n -> int_of_string (Js.to_string n))

(* [select_time_handler elt] add a click handler to [elt] to indicate select
 * and deselect of a timeslot *)
let%client select_time_handler elt =
  let dom = Html.To_dom.of_element elt in
  let h = get_attribute "data-height" dom in
  let w = get_attribute "data-width" dom in
  let has_selected = Js.to_bool
      (dom##.classList##contains (Js.string "selected")) in
  let%lwt old_remove_list =
    Eliom_cscache.find ~%event_cache get_cache "remove" in
  let%lwt old_add_list =
    Eliom_cscache.find ~%event_cache get_cache "add" in
  if has_selected
  then
    let _ = dom##.classList##remove (Js.string "selected") in
    let new_remove_list = (w,h)::old_remove_list in
    let new_add_list = List.filter ((!=) (w,h)) old_add_list in
    Lwt.return(
      let _ = Eliom_cscache.do_cache ~%event_cache "remove" new_remove_list in
      Eliom_cscache.do_cache ~%event_cache "add" new_add_list
    )
  else
    let _ = dom##.classList##add (Js.string "selected") in
    let new_add_list = (w,h)::old_add_list in
    let new_remove_list = List.filter ((!=) (w,h)) old_remove_list in
    Lwt.return(
      let _ =
        Eliom_cscache.do_cache ~%event_cache "remove" new_remove_list in
      (Eliom_cscache.do_cache ~%event_cache "add" new_add_list) : unit
    )

(* [inspect_handler_mouse_over elt users] add a mouse over handler to display
 * [users] in the inspection table *)
let%client inspect_handler_mouse_over elt users =
  let dom = Html.To_dom.of_element elt in
  let h = get_attribute "data-height" dom in
  let w = get_attribute "data-width" dom in
  let entries =
    List.map (fun user_id -> td [pcdata user_id]) users in
  let inspect_table =
    Js.Opt.get
      (Dom_html.document##getElementById (Js.string "inspect"))
      (fun () -> failwith "did not find inspect") in
  let append_entry entry =
    (Dom.appendChild
	       (inspect_table) (Html.To_dom.of_element entry)
               : unit) in
  let _ = List.map append_entry entries in
  Lwt.return ()

(* [num_handler_mouse_over elt users] add a mouse over handler to display
 * length of [users] in the inspection table *)
let%client num_handler_mouse_over elt users =
  let dom = Html.To_dom.of_element elt in
  let h = get_attribute "data-height" dom in
  let w = get_attribute "data-width" dom in
  let size =
    List.length users in
  let num_table =
    Js.Opt.get
      (Dom_html.document##getElementById (Js.string "num"))
      (fun () -> failwith "did not find num") in
  let append_entry entry =
    (Dom.appendChild
       (num_table) entry
     : unit) in
  let t1 = Dom_html.document##createTextNode(Js.string (string_of_int size)) in
  let _ = append_entry t1 in
  Lwt.return ()

[%%client
let rec clear_children dom =
  let first = dom##.firstChild in
    Js.Opt.case
      first
      (fun () -> ())
      (fun child ->
         let _ = (Dom.removeChild dom child) in
         clear_children dom)
]

(* [inspect_handler_mouse_out] add a mouse out handler to elt to clear
 * the content of inspection table*)
let%client inspect_handler_mouse_out elt =
  let dom = Html.To_dom.of_element elt in
  let h = get_attribute "data-height" dom in
  let w = get_attribute "data-width" dom in
  let inspect_table =
    Js.Opt.get
      (Dom_html.document##getElementById (Js.string "inspect"))
      (fun () -> failwith "did not find inspect") in
  clear_children inspect_table

(* [num_handler_mouse_out] add a mouse out handler to elt to clear
 * the content of num table*)
let%client num_handler_mouse_out elt =
  let dom = Html.To_dom.of_element elt in
  let h = get_attribute "data-height" dom in
  let w = get_attribute "data-width" dom in
  let num_table =
    Js.Opt.get
      (Dom_html.document##getElementById (Js.string "num"))
      (fun () -> failwith "did not find num") in
  clear_children num_table



let filter_select has_been_selected f =
  List.filter (fun (u, _, _) -> f u) has_been_selected

(* [add_handlers]*)
let add_handlers bt users=
        let _ = [%client
        (Lwt_js_events.(Lwt.async (fun () ->
           clicks (Html.To_dom.of_element ~%bt)
             (fun _ _ ->
                let _ =
                  select_time_handler (~%bt) in Lwt.return ()))) : unit)] in
        let _ = [%client
        (Lwt_js_events.(Lwt.async (fun () ->
           mouseovers (Html.To_dom.of_element ~%bt)
             (fun _ _ ->
                let _ = (inspect_handler_mouse_over (~%bt) (~%users)) in
                Lwt.return ()))) : unit)
        ] in
        let _ = [%client
        (Lwt_js_events.(Lwt.async (fun () ->
           mouseouts (Html.To_dom.of_element ~%bt)
             (fun _ _ ->
                let _ =
                  (inspect_handler_mouse_out (~%bt) ) in
                Lwt.return ()))) : unit)
        ] in let _ = [%client
             (Lwt_js_events.(Lwt.async (fun () ->
                mouseovers (Html.To_dom.of_element ~%bt)
                  (fun _ _ ->
                     let _ = (num_handler_mouse_over (~%bt) (~%users)) in
                     Lwt.return ()))) : unit)
             ] in
        let _ = [%client
        (Lwt_js_events.(Lwt.async (fun () ->
           mouseouts (Html.To_dom.of_element ~%bt)
             (fun _ _ ->
                let _ =
                  (num_handler_mouse_out (~%bt) ) in
                Lwt.return ()))) : unit)
        ] in
        ()

(* [signup_msg] is the message to prompt user in the sign up page if the user
 * fails to sign up. set to empty at start.
 * [example]: "Submitted" *)
let submit_msg = Eliom_reference.eref
    ~scope:Eliom_common.default_process_scope ""

let timetable event_id user_id =
  let width = 7 in
  let height = 14 in
  let has_been_selected = Database.timeslots_of_event event_id in
  let my_selected = filter_select has_been_selected ((=) user_id) in
  let other_selected = filter_select has_been_selected ((<>) user_id) in
  let start_time = Database.starttime_of_event event_id in
  let rec make_column h acc_col =
    let rec make_row w acc_row =
      if w == 0 then acc_row
      else
        let ts = Time.string_of_relative_hour h start_time in
        let classlist =
          if List.exists ((=) (user_id, w, h)) my_selected
          then ["timeslot"; "selected"]
          else ["timeslot";] in
        let classlist' =
          if List.exists (fun (n, w', h') -> (w', h') = (w, h))
              other_selected then "other-selected"::classlist
          else classlist in
        let bt = td ~a:[a_class classlist';
                        a_user_data "height" (string_of_int h);
                        a_user_data "width" (string_of_int w)] [pcdata ts] in
        let users = has_been_selected |>
                    List.filter (fun (n, w', h') -> (w', h') = (w, h)) |>
                    List.map (fun (n, w', h') -> n) in
        let () = add_handlers bt users in
        make_row (w-1) (bt::acc_row) in
    if h == 0 then acc_col
    else
      let row = tr (make_row width []) in make_column (h-1) (row::acc_col)
  in
  let rec make_head d acc_head =
    if d == 0 then tr acc_head
    else let day = th [pcdata (Time.string_of_relative_day d start_time)] in
      make_head (d-1) (day::acc_head)
  in
  div [table ((make_head 7 [])::(make_column height []));]

[%%shared
  type submit_type =
    | Add of string * string * (int * int) list
    | Remove of string * string * (int * int) list
    [@@deriving json]
]

(*[submit_timeslots slots] submit the remove and add list in the cache*)
let submit_timeslots submit_content =
  let _ =
  match submit_content with
  | Add (u,e,slots) -> Database.add_timeslot u e slots
  | Remove (u,e,slots) ->
    let _ = List.map (Database.remove_timeslot u e) slots in ()
  in
  Lwt.return ()

let submit_timeslots_rpc =
  Eliom_client.server_function [%derive.json: submit_type] (fun s ->
  submit_timeslots s)

(* [submit_timeslots user_id event_id] submit all the timeslot to be added or
 * removed in the cache to server *)
let%client submit_timeslots user_id event_id =
  let%lwt remove_list =
    Eliom_cscache.find ~%event_cache get_cache "remove" in
  let%lwt add_list =
    Eliom_cscache.find ~%event_cache get_cache "add" in
  let%lwt _ =
    ~%submit_timeslots_rpc (Add (user_id, event_id, add_list)) in
  let%lwt _ =
    ~%submit_timeslots_rpc (Remove (user_id, event_id, remove_list)) in
  (Lwt.return())

(* [inspect_handler_mouse_out] add a mouse out handler to elt to clear
 * the content of inspection table*)
let%client submit_handler_click elt =
  let dom = Html.To_dom.of_element elt in
  let inspect_area =
    Js.Opt.get
      (Dom_html.document##getElementById (Js.string "txt-area"))
      (fun () -> failwith "did not find inspect") in
  let message = div [p [pcdata "Your selection has been recorded"]] in
    (Dom.appendChild
	       (inspect_area) (Html.To_dom.of_element message)
               : unit)

(* [submitbutton event_id user_id] is a html button component to will call
 * [submit_timeslots user_id event_id] when clicked*)
let submitbutton event_id user_id =
  let bt = button ~a:[a_class ["submitbutton"];] [pcdata "Submit"] in
  let _ = [%client
  (Lwt_js_events.(Lwt.async (fun () ->
     clicks (Html.To_dom.of_element ~%bt)
       (fun _ _ ->
          let _ = submit_handler_click (~%bt) in Lwt.return ()))) : unit)] in
  let _ = [%client
  (Lwt_js_events.(Lwt.async (fun () -> clicks (Html.To_dom.of_element ~%bt)
       (fun _ _ -> let%lwt _ = submit_timeslots ~%user_id ~%event_id in
         Lwt.return ()))) : unit) ] in
  bt
