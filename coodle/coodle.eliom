[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Eliom_service
]

(* [connected_user] is the username if a user login in this session. otherwise,
 * it is an empty string*)
let connected_user = Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope ""

(* [msg] is the message to prompt user in the login page if the user fails to
 * login. set to empty at start.
 * [example]: "Wrong Password" it is an empty string*)
let msg = Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope ""

(* [signup_msg] is the message to prompt user in the sign up page if the user
 * fails to sign up. set to empty at start.
 * [example]: "Username already taken" *)
let signup_msg = Eliom_reference.eref
    ~scope:Eliom_common.default_process_scope ""

(* [new_event_msg] is the message to prompt user in the create event page if
 * the user fails to sign up. set to empty at start.
 * [example]: "Empty event name" *)
let new_event_msg = Eliom_reference.eref
    ~scope:Eliom_common.default_process_scope ""

(* [current_eventname] is the name of event that is being operating by the user
 * the user fails to sign up. set to empty at start. *)
let current_eventname = Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope ""

(* [invite_msg] is the message to prompt user in the invite people page if
 * the user fails to invite. set to empty at start.
 * [example]: "User does not exist" *)
let invite_msg = Eliom_reference.eref
    ~scope:Eliom_common.default_process_scope ""

(* [set_msg] is the message to prompt user in the set manager page if
 * the user fails to set manager. set to empty at start.
 * [example]: "User does not exist" *)
let set_msg = Eliom_reference.eref
    ~scope:Eliom_common.default_process_scope ""

(* [main_service] is the login page*)
let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

(* [login_service] post to server with username and password in order to login *)
let login_service =
  Eliom_service.create_attached_post
    ~fallback: main_service
    ~post_params: (Eliom_parameter.(string "username" ** string "password"))
    ()

(* [signup_service] get from the server the page to sign up*)
let signup_service =
  Eliom_service.create
  ~path:(Eliom_service.Path ["signup"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

(* [create_account_service] post to server with username and password in order
 * to create a new account.*)
let create_account_service =
  Eliom_service.create_attached_post
  ~fallback:signup_service
  ~post_params:(Eliom_parameter.(string "username" ** string "password" ))
  ()

(* [logout_service] post to server to clear session user data*)
let logout_service =
  Eliom_service.create_attached_get
    ~fallback:main_service
    ~get_params:(Eliom_parameter.unit)
    ()

(* [new_event_service] get from server the page to create a new user*)
let new_event_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["newevent"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

(* [create_event_service] post to server with event name, event start time,
 * and event description*)
let create_event_service =
  Eliom_service.create_attached_post
    ~fallback:new_event_service
    ~post_params:(Eliom_parameter.(string "eventname" **
                                   (string "time" **
                                    (string "desc" ** bool "box"))))
    ()

(* [invite_ppl_service] get from service the page to invite people into a event*)
let invite_ppl_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["invite"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

(* [set_manager_ppl_service] get from server the page to set manager*)
let set_manager_ppl_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["setting"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

(* [invite_service] post to server to finalize invitation*)
let invite_service = Eliom_service.create_attached_post
    ~fallback: invite_ppl_service
    ~post_params:(Eliom_parameter.(string "invite" ))
    ()

(* [set_manager_service] post to server to confirm admin access setting*)
let set_manager_service = Eliom_service.create_attached_post
    ~fallback: set_manager_ppl_service
    ~post_params:(Eliom_parameter.(string "Confirm" ))
    ()

(* [finalize_service] post to server to set the event as final*)
let finalize_service =
  Eliom_service.create
    ~path:(Eliom_service.No_path)
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    ()
(* [quit_service] post to server to set the event as final*)
let quit_service =
  Eliom_service.create
    ~path:(Eliom_service.No_path)
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    ()

(* [schedule_service] get the page to schedule service*)
let schedule_service =
    Eliom_service.create
      ~path:(Eliom_service.Path ["myschedule"])
      ~meth:(Eliom_service.Get Eliom_parameter.unit)
      ()
(* [past_service] get the page to past service*)
let past_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["past"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()


(* helper module to create a web application service*)
module Event_app =
  Eliom_registration.App
    (struct
      let application_name = "Coodle"
      let global_data_path = None
    end)

(* [event_service] get from the server the main event page*)
let event_service =
  Event_app.create
    ~path:(Eliom_service.Path ["event"])
    ~meth:(Eliom_service.Get (
      Eliom_parameter.(suffix (string "eventname"))))
    (fun eventname () ->
       let en = Eliom_reference.set current_eventname (eventname) in
       let%lwt u = Eliom_reference.get connected_user in
       let%lwt result = match u with
         | "" ->
           Lwt.return (
             div [
               h1 [pcdata "Access Denied"];
               p [a ~service:main_service [pcdata "Home"] ()]]
           )
         | _  ->
           let title = h2 [pcdata eventname] in
           let desc = "Description: "^(Database.event_desc eventname) in
           let desc_html = p [pcdata desc] in
           let user_line =
             div ~a:[a_class ["c_links"]]
               [p [a ~service: main_service [pcdata "Home"] ()]] in
           let invite_line =
             div ~a:[a_class ["c_links"]]
               [ p [a ~service: invite_ppl_service [pcdata "Invite"] ()]] in
           let manage_line =
             div ~a:[a_class ["c_links"]]
               [ p [a ~service:
                      set_manager_ppl_service [pcdata "Add Managers"] ()]] in
           let logout =
             div ~a:[a_class ["c_links"]]
               [p [a ~service:logout_service [pcdata "Log out"] ()]] in
           let option_btn =
             if Database.if_admin u eventname
             then  div ~a:[a_class ["sub_table"]] [Widget.submitbutton eventname u;
                                                   Widget.finalize_form finalize_service]
             else  div ~a:[a_class ["sub_table"]] [Widget.submitbutton eventname u;
                                                   Widget.quit_form quit_service] in
           let button_list =
             match Database.status_of_event eventname with
             | Open ->
               if Database.if_admin u eventname
               then
                 [user_line; invite_line; manage_line; logout;]
               else if Database.if_manager u eventname
               then
                 [user_line; invite_line; logout;]
                 (* else if Database.if_admin u eventname *)
               else
                 [user_line;
                  logout;
                 ]
             | Finalized (_,_) -> [user_line;logout]
           in
           let btn_line =
             div ~a:[a_class ["c_menu"]] button_list
           in
           let inp_table = div ~a:[a_class ["sub_table"]]
               [Widget.inspection_table eventname u] in
           let status = Database.status_of_event eventname in
           let result =
             match status with
             | Open ->
               div[
                 btn_line;
                 div [title];
                 div [desc_html];
                 div [Widget.timetable eventname u];
                 div ~a:[a_class ["table"]] [option_btn;inp_table]]
             | Finalized (day,time)->
               let start_time =
                 Database.starttime_of_event eventname in
               let str =
                 "Finalized Meeting Time: "^
                 (Time.string_of_relative_day day start_time) ^ " " ^
                 (Time.string_of_relative_hour time start_time) in
               div[btn_line; div[title]; div[desc_html]; p [pcdata str]] in
           Lwt.return (result)
       in
       Lwt.return(
         Eliom_tools.F.html ~title:eventname
           ~css:[["css";"coodle.css"]] ( body [result] )
       )
    )

(* [event_links users] is a html list component that displays event page
 * hyperlink *)
let event_links users =
  Eliom_content.Html.D.(
    let link_of_event =
      fun (eventname) ->
        li [a ~service:event_service [pcdata eventname] eventname] in
    ul (List.map link_of_event users)
)

(* register signup_service with page content *)
let _ = Eliom_content.Html.D.(
  Eliom_registration.Html.register
    ~service:signup_service
    (fun () () ->
       let%lwt msg = Eliom_reference.get signup_msg in
       Lwt.return (
       let display =
         match msg with
         | "" -> div [Widget.user_pwd_form create_account_service "Sign up";
                       p [a main_service [pcdata "Back"] ()]]
         | _  -> div [
           p [pcdata msg];
           Widget.user_pwd_form create_account_service "Sign up";
           p [a main_service [pcdata "Back"] ()] ]
       in
       Eliom_tools.F.html
         ~title:"Create an account" ~css:[["css";"coodle.css"]]
         (body [h1 [pcdata "Create an account"]; display]))
     )
    )


(* [user_lst users] is a html list component that displays users *)
let user_lst users =
  Eliom_content.Html.D.(
    let lst_of_user =
      fun (username) ->
        li [p [pcdata username]] in
    ul (List.map lst_of_user users)
  )

(* register invite_ppl_service with page content *)
let _ = Eliom_content.Html.D.(
  Eliom_registration.Html.register
    ~service:invite_ppl_service
    (fun () () ->
       let%lwt msg = Eliom_reference.get invite_msg in
       let%lwt u = Eliom_reference.get connected_user in
       let %lwt event_name = Eliom_reference.get current_eventname in
       Lwt.return (
         let cur_users = Database.users_of_event event_name in
         let display =
           match msg with
           | "" -> div [Widget.setting_form invite_service "Invite";
                        user_lst cur_users;
                        p [a event_service [pcdata "Back"] event_name]]
           | _  -> div [
             p [pcdata msg];
             Widget.setting_form invite_service "Invite" ;
             user_lst cur_users;
             p [a event_service [pcdata "Back"] event_name] ]
         in
         let result =
           match u with
           | "" -> [h1 [pcdata "Please log in first"];
                    p [a ~service:main_service [pcdata "Home"] ()]]
           | _ -> [h1 [pcdata "Invite new user"]; display] in
         Eliom_tools.F.html
           ~title:"Invite" ~css:[["css";"coodle.css"]]
           (body result ) )

    )
)

(* register set_manager_ppl_service with page content *)
let _ = Eliom_content.Html.D.(
  Eliom_registration.Html.register
    ~service:set_manager_ppl_service
    (fun () () ->
       let%lwt msg = Eliom_reference.get set_msg in
       let%lwt u = Eliom_reference.get connected_user in
       let %lwt event_name = Eliom_reference.get current_eventname in
       Lwt.return (
         let cur_manager = Database.managers_of_event event_name in
         let display =
           match msg with
           | "" -> div [Widget.setting_form set_manager_service "Confirm";
                        user_lst cur_manager;
                        p [a event_service [pcdata "Back"] event_name]]
           | _  -> div [
             p [pcdata msg];
             Widget.setting_form set_manager_service "Confirm" ;
             user_lst cur_manager;
             p [a event_service [pcdata "Back"] event_name] ]
         in
         let result =
           match u with
           | "" -> [h1 [pcdata "Please log in first"];
                    p [a ~service:main_service [pcdata "Home"] ()]]
           | _ -> [h1 [pcdata "Add managers"]; display] in
         Eliom_tools.F.html
           ~title:"Add managers" ~css:[["css";"coodle.css"]]
           (body result ) )
    )
)

(* register new_event_service with page content *)
let _ =
  Eliom_content.Html.D.(
  Eliom_registration.Html.register
    ~service:new_event_service
    (fun () () ->
       let%lwt msg = Eliom_reference.get new_event_msg in
       let%lwt u = Eliom_reference.get connected_user in
       Lwt.return (
         let display =
           match msg with
           | "" -> div[Widget.create_event_form create_event_service;
                       p [a main_service [pcdata "Back"] ()] ]

           | _  -> div[
             p [pcdata msg];
             Widget.create_event_form create_event_service;
             p [a main_service [pcdata "Back"] ()]] in
         let result =
           match u with
           | "" -> [h1 [pcdata "Please log in first"];
                    p [a ~service:main_service [pcdata "Home"] ()]]
           | _ -> [h1 [pcdata "Create a new event"]; display] in
         Eliom_tools.F.html
           ~title:"Create new event" ~css:[["css";"coodle.css"]]
            (body result ) )
       );
)
  (*filters out events that happened in the past*)
let filter_time event =
match Database.status_of_event event with
| Finalized (day, time) -> begin
    let diff = ((float_of_int (time-1)) *. 60. *. 60. )+.
                ((float_of_int (day-1)) *. 60. *. 60. *. 24.) in
    let starttime = Database.starttime_of_event event +. diff in
    let cur_time = Unix.time() in
    starttime>cur_time

  end
| _-> failwith "impossible"


(*Comparator used for sorting the events *)
let sort_time event1 event2 =
  match Database.status_of_event event1,
        Database.status_of_event event2 with
  | Finalized (day1, time1),
    Finalized(day2, time2) -> begin
      let diff1 = ((float_of_int (time1-1)) *. 60. *. 60. )+.
                  ((float_of_int (day1-1)) *. 60. *. 60. *. 24.) in
      let diff2 = ((float_of_int (time2-1)) *. 60. *. 60. )+.
                  ((float_of_int (day2-1)) *. 60. *. 60. *. 24.) in
      let starttime1 = Database.starttime_of_event event1 +. diff1 in
      let starttime2 = Database.starttime_of_event event2 +. diff2 in
      Pervasives.compare starttime1 starttime2
    end
  | _, _ -> failwith "impossible"

(* [finalized_lst] is a html list component that displays all finalized event *)
let finalized_lst (e:string list) =
  Eliom_content.Html.D.(
    let lst_of_e =
      fun (eventname) ->
        match Database.status_of_event eventname with
        | Open -> failwith "impossible"
        | Finalized (day,time)->
          begin
            let start_time = Database.starttime_of_event eventname in
            let event_time =
              eventname ^ ": " ^
              (Time.string_of_relative_day day start_time) ^
              " " ^ (Time.string_of_relative_hour time start_time)in
            li [p [pcdata event_time]]
          end in
    let e_filter = List.filter filter_time e in
    let e_sort = List.sort sort_time e_filter in
    ul (List.map lst_of_e e_sort)
  )

(* [finalized_lst] is a html list component that displays all finalized event *)
let finalized_past_lst (e:string list) =
  Eliom_content.Html.D.(
    let lst_of_e =
      fun (eventname) ->
        match Database.status_of_event eventname with
        | Open -> failwith "impossible"
        | Finalized (day,time)->
          begin
            let start_time = Database.starttime_of_event eventname in
            let event_time =
              eventname ^ ": " ^
              (Time.string_of_relative_day day start_time) ^
              " " ^ (Time.string_of_relative_hour time start_time)in
            li [p [pcdata event_time]]
          end in
    let e_filter = List.filter (fun x -> not(filter_time x)) e in
    let e_sort = List.sort sort_time e_filter in
    ul (List.map lst_of_e e_sort)
  )

(* register schedule_service with page content *)
let _ =
  Eliom_content.Html.D.(
    Eliom_registration.Html.register
      ~service:schedule_service
      (fun () () ->
         let%lwt u = Eliom_reference.get connected_user in
         Lwt.return (
           let elst = Database.finalized_event u in
           let display =
             div[finalized_lst elst;
                 p [a past_service [pcdata "Past Events"] ()];
                          p [a main_service [pcdata "Back"] ()]] in
           let result =
             match u with
             | "" -> [h1 [pcdata "Please log in first"];
                      p [a ~service:main_service [pcdata "Home"] ()]]
             | _ -> [h1 [pcdata "My schedule"]; display] in
           Eliom_tools.F.html
             ~title:"My schedule" ~css:[["css";"coodle.css"]]
             (body result ) )
      );
  )

(* register past_service with page content *)
let _ =
  Eliom_content.Html.D.(
    Eliom_registration.Html.register
      ~service:past_service
      (fun () () ->
         let%lwt u = Eliom_reference.get connected_user in
         Lwt.return (
           let elst = Database.finalized_event u in
           let display =
             div[finalized_past_lst elst;
                 p [a main_service [pcdata "Back"] ()]] in
           let result =
             match u with
             | "" -> [h1 [pcdata "Please log in first"];
                      p [a ~service:main_service [pcdata "Home"] ()]]
             | _ -> [h1 [pcdata "Past Events"]; display] in
           Eliom_tools.F.html
             ~title:"Past Events" ~css:[["css";"coodle.css"]]
             (body result ) )
      );
  )


(* register create_account_service with page content *)
let _ = Eliom_content.Html.D.(
  Eliom_registration.Any.register
    ~service:create_account_service
    (fun () (name, pwd) ->
       let name_trim = String.trim name in
       if name_trim = ""
       then let _ =
              Eliom_reference.set signup_msg "Username can not be empty." in
         Eliom_registration.Action.send ()
       else if pwd = ""
       then let _ =
              Eliom_reference.set signup_msg "Password can not be empty." in
         Eliom_registration.Action.send ()
       else if Database.username_unique name_trim then
        let _ =
          Database.add_new_account name_trim pwd in
        let _ =
          Eliom_reference.set msg "Success, Login with your account" in
        let _ =
          Eliom_reference.set signup_msg "" in
        Eliom_registration.Redirection.send
          (Eliom_registration.Redirection main_service)
      else
        let _ =
          Eliom_reference.set signup_msg "Username has been taken." in
        Eliom_registration.Action.send ()
    )
)

(* register create_service with page content *)
let _ = Eliom_content.Html.D.(
  Eliom_registration.Any.register
    ~service:create_event_service
    (fun () (name,(time,(desc,anon))) ->
       let%lwt u = Eliom_reference.get connected_user in
       let name_trim = String.trim name in
       if name_trim = "" then
         let _ =
           Eliom_reference.set new_event_msg "Event name can not be empty." in
         Eliom_registration.Action.send ()
       else if time = "" then
         let _ =
           Eliom_reference.set new_event_msg "Invalid event start time." in
         Eliom_registration.Action.send ()
       else if Database.eventname_unique name_trim then
         let _ =
           let t = Time.float_of_string_start time in
         Database.add_new_event u name_trim desc [] t anon in
         let _ =
           Eliom_reference.set new_event_msg "" in
         Eliom_registration.Redirection.send
           (Eliom_registration.Redirection main_service)
       else
         let _ =
           Eliom_reference.set new_event_msg "Event name has been taken." in
         Eliom_registration.Action.send ()
    )
)

(* register invite_service with page content *)
let _ =
  Eliom_registration.Action.register
    ~service:invite_service
    (fun () (username) ->
       let name_trim = String.trim username in
       let%lwt e = Eliom_reference.get current_eventname in
       let _ =
         if List.mem name_trim (Database.users_of_event e)
         then Eliom_reference.set invite_msg
             ("User already exists in current event")
         else if Database.check_user_valid name_trim
         then let _ =
           Database.invite e [name_trim] in
           Eliom_reference.set invite_msg ("Success")
         else
           Eliom_reference.set invite_msg ("No such user") in
       Lwt.return())

(* register set_manager_service with page content *)
let _ =
  Eliom_registration.Action.register
    ~service:set_manager_service
    (fun () (username) ->
       let name_trim = String.trim username in
       let%lwt e = Eliom_reference.get current_eventname in
       let _ =
         if not(List.mem name_trim (Database.users_of_event e))
         then
           Eliom_reference.set set_msg
             ("User Does not exist in current event")
         else if Database.if_admin name_trim e
         then
           Eliom_reference.set set_msg
             ("Unauthorized Action")
         else
           let _ =
             Database.change_admin name_trim e "manager" in
           Eliom_reference.set set_msg ("Success") in
       Lwt.return())

(* Log in action handler*)
let _ =
  Eliom_registration.Action.register
    ~service:login_service
    (fun () (name,pwd) ->
       let _ =
       if Database.check_password name pwd
       then let _ =
         Eliom_reference.set msg ("") in
         Eliom_reference.set connected_user (name)
       else
         Eliom_reference.set msg ("Wrong Password") in
      Lwt.return())

(* register logout_service with page content *)
let _ =
  Eliom_registration.Action.register
    ~service:logout_service
    (fun () () ->
       Eliom_state.discard
         ~scope:Eliom_common.default_session_scope ())

(* register finalize_service with page content *)
let _ =
  Eliom_registration.Action.register
    ~service:finalize_service
    (fun () () ->
       let%lwt e = Eliom_reference.get current_eventname in
       let _ = Database.finalize e in
       Lwt.return())

(* register quit_service with page content *)
let _ =
Eliom_content.Html.D.(
  Eliom_registration.Any.register
    ~service:quit_service
    (fun () () ->
       let%lwt e = Eliom_reference.get current_eventname in
       let%lwt u = Eliom_reference.get connected_user in
       let _ = Database.quit u e in
       Eliom_registration.Redirection.send
         (Eliom_registration.Redirection main_service)))



(* [major_dom ()] is the main content of the main page *)
let major_dom () =
  let%lwt u = Eliom_reference.get connected_user in
  Lwt.return (
    match u with
    | "" ->
      div [Widget.user_pwd_form login_service "Log in";
           p [a signup_service [pcdata "Create an account"] ()]]
    | _ ->
      let e = Database.events_of_user u in
      div [
        div ~a: [a_class ["e_links"]]
          [event_links e];
      ]
  )

(* register main_service with page content *)
let _ =
  Eliom_registration.Html.register
      ~service:main_service
      (fun () () ->
         let%lwt m = Eliom_reference.get msg in
         let%lwt u = Eliom_reference.get connected_user in
         let%lwt d = major_dom () in
         Lwt.return (
           let logout_menu =
             div ~a:[a_class ["header"]]
               [p [a ~service:logout_service
                     [pcdata "Log out"] ()]] in
           let sche_menu =
             div ~a:[a_class ["header"]]
               [p [a ~service:schedule_service
                     [pcdata "My Schedule"] ()]] in
           let newevent_menu =
             div ~a:[a_class ["header"]]
               [p [a ~service:new_event_service
                     [pcdata "Create new Event"] ()]]
           in
           let header_div =
             match u with
             | "" -> div []
             | _ ->
               div ~a:[a_class ["menu"]]
                 [sche_menu; newevent_menu;logout_menu]
           in
           let head =
             (match u with
             | "" ->
               let welcome = "Welcome to Coodle!" in
               h1 ~a: [a_class ["w_msg"]] [pcdata welcome]
             | _  ->
               let events = "Hello, "^u^". Here are your events" in
               h1 ~a: [a_class ["e_msg"]] [pcdata events]
             )
           in
           let result =
                match m with
                | "" -> [d]
                | _ ->  div [p [pcdata m]] :: [d] in
           Eliom_tools.F.html ~title:"Coodle" ~css:[["css";"login.css"]]
             (body [
                div [header_div];
                head;
                div ~a: [a_class ["result"]] result;
           ])
         ) )
