open Printf
exception DataError
type admin_right = Admin | Manager | User
type status = Open | Finalized of (int * int)
(* print one row *)
let print_row i row =
  printf "row %d: [%s]\n" i
    (String.concat "; "
       (List.map (function
          | None -> "NULL"
          | Some str -> sprintf "%S" str) row))

(* print a list of row *)
let print_rows rows =
  List.iteri print_row rows

(* connect to database *)
let connect () =
  PGOCaml.connect
    ~host:"localhost"
    ~user:"coodle"
    ~database:"coodle_data"
    ~password:"coodle3110" ()

(* [exec dbh query] executes [query] on the database [dbh] *)
let exec dbh query =
  PGOCaml.prepare dbh ~query ();
  let ans = PGOCaml.execute dbh ~params:[] () in
  ans

let wrap s = "\'" ^ s ^ "\'"

let check_user_valid usrid =
  let db = connect () in
  let result = exec db
    ("select * from account where aid =" ^ (wrap usrid)) in
  PGOCaml.close db;
  match result with
  | [] -> false
  | _ -> true

let check_event_valid eventid =
  let db = connect () in
  let result = exec db
    ("select * from event where eid =" ^ (wrap eventid)) in
  PGOCaml.close db;
  match result with
  | [] -> false
  | _ -> true

let event_desc eventid =
  let db = connect () in
  let result = exec db
      ("select description from event where eid ="
       ^ (wrap eventid)) in
  PGOCaml.close db;
  match result with
  | [Some des]::[] -> des
  | _ -> "no description"

let events_of_user accountid =
  let db = connect () in
  let result = exec db
      ("select eid from admin where aid ="
       ^ (wrap accountid)) in
  PGOCaml.close db;
  (List.map (function
     | [Some str] -> str
     | _ -> "no such account or event") result)

let users_of_event eventid =
  let db = connect () in
  let result = exec db
      ("select aid from admin where eid ="
       ^ (wrap eventid)) in
  PGOCaml.close db;
  (List.map (function
     | [Some str] -> str
     | _ -> "no such account or event") result)

let user_event_right eventid accountid =
  let db = connect () in
  let result = exec db
      ("select admin_right from admin where aid ="
       ^ (wrap accountid) ^ "and eid ="
       ^ (wrap eventid)) in
  PGOCaml.close db;
  match result with
  | [[Some "admin"]] -> Admin
  | [[Some "user"]] -> User
  | [[Some "manager"]] -> Manager
  | _ -> raise DataError

let add_new_account usrname pwd =
  let db = connect () in
  let _ = exec db ("insert into account values ("
                   ^ (wrap usrname) ^ ","
                   ^ (wrap pwd) ^ ")") in
  PGOCaml.close db

let username_unique usrname =
  let db = connect () in
  let result =
    exec db ("select * from account where aid ="
             ^ (wrap usrname)) in
  PGOCaml.close db;
  match result with
  | [] -> true
  | _ -> false

let eventname_unique eventname =
  let db = connect () in
  let result =
    exec db ("select * from event where eid ="
             ^ (wrap eventname)) in
  PGOCaml.close db;
  match result with
  | [] -> true
  | _ -> false

let check_password usrname pwd =
  let db = connect () in
  let result = exec db
      ("select pwd from account where aid ="
       ^ (wrap usrname)) in
  PGOCaml.close db;
  match result with
  | [[Some str]] -> str = pwd
  | _ -> false

let if_admin usrid eid =
  let db = connect () in
  let result = exec db
      ("select admin_right from admin where aid =" ^ (wrap usrid)
       ^ " and " ^ "eid =" ^ (wrap eid)) in
  PGOCaml.close db;
  match result with
  | [[Some str]] -> str = "admin"
  | _ -> raise DataError

let if_manager usrid eid =
  let db = connect () in
  let result = exec db
      ("select admin_right from admin where aid =" ^ (wrap usrid)
       ^ " and " ^ "eid =" ^ (wrap eid)) in
  PGOCaml.close db;
  match result with
  | [[Some str]] -> str = "manager"
  | _ -> raise DataError

let managers_of_event eventid =
  let db = connect () in
  let result = exec db ("select aid from admin where eid ="
                        ^ (wrap eventid)
                        ^ " and admin_right =" ^ (wrap "manager")) in
  PGOCaml.close db;
  (List.map (function
     | [Some str] -> str
     | _ -> raise DataError) result)

let all_events usrid =
  let db = connect () in
  let result = exec db ("select eid from admin where aid ="
                        ^ (wrap usrid)) in
  PGOCaml.close db;
  (List.map (function
     | [Some str] -> str
     | _ -> "no such account or event") result)

let add_new_event usrid eventid desc invite start_time is_anonymous =
  let db = connect () in
  let result = exec db
      ("insert into event (eid, description, status, start_time, anonymous)"
       ^ "values ("
       ^ (wrap eventid) ^ ","
       ^ (wrap desc) ^ ","
       ^ (wrap "undecided") ^ ","
       ^ (string_of_float start_time) ^ ","
       ^ (string_of_bool is_anonymous)
       ^ ")");
    exec db ("insert into admin (aid,eid,admin_right) values" ^ "("
             ^ (wrap usrid) ^ ","
             ^ (wrap eventid) ^ ","
             ^ (wrap "admin") ^ ")");

    (let rec add_mem eid = function
       | [] -> ()
       | h::t -> exec db
                   ("insert into admin (aid,eid,admin_right) values" ^"("
                    ^ (wrap h) ^ ","
                    ^ (wrap eid) ^ ","
                    ^ (wrap "user") ^ ")");
      add_mem eid t in
      add_mem eventid invite) in
  PGOCaml.close db

let add_timeslot usrid eventid slots =
  let db = connect () in
  let rec add_slot = function
    | [] -> ()
    | (day, time)::t ->
      let res = exec db ("select * from availability where aid =" ^ (wrap usrid)
                ^ " and eid =" ^ (wrap eventid)
                ^ " and day =" ^ (string_of_int day)
                ^ " and time =" ^ (string_of_int time)) in
      (match res with
      | [] ->
      (exec db
        ("insert into availability (aid, eid, day, time) values" ^ "("
         ^ (wrap usrid) ^ ","
         ^ (wrap eventid) ^ ","
         ^ (string_of_int day) ^ ","
         ^ (string_of_int time) ^ ")");
      add_slot t 
      )
      | _ -> add_slot t) in
  add_slot slots;
  PGOCaml.close db

let timeslots_of_event eventid =
  let db = connect () in
  let result = exec db ("select aid, day, time from availability where eid = "
                        ^ (wrap eventid)) in
  let result2 = exec db ("select anonymous from event where eid = "
                        ^ (wrap eventid)) in
  let is_anony =
    (match result2 with
    | [[Some "t"]] -> true
    | [[Some "f"]] -> false
    | _ -> raise DataError) in
  PGOCaml.close db;
  (List.map (function
     | (Some usr)::(Some day)::(Some time)::[] ->
       if (is_anony) then ("Anonymous", int_of_string day, int_of_string time)
       else (usr, int_of_string day, int_of_string time)
     | _ -> raise (DataError)) result)

let user_on_slot_of_event (day, time) eventid =
  let db = connect () in
  let result = exec db ("select distinct aid from availability where eid = "
                        ^ (wrap eventid)
                        ^ " and day =" ^ wrap (string_of_int day)
                        ^ " and time =" ^ wrap (string_of_int time)) in
  PGOCaml.close db;
  (List.map (function
     | [(Some usr)] -> usr
     | _ -> raise (DataError)) result)

let starttime_of_event eventid =
  let db = connect () in
  let result = exec db ("select start_time from event where eid = "
                       ^ (wrap eventid)) in
  PGOCaml.close db;
  match result with
  | [[Some str]] -> float_of_string str
  | _ -> raise (DataError)

let invite eid usrlist =
  let db = connect () in
  let rec add_usr = function
    | [] -> ()
    | usrid::t ->
      exec db ("insert into admin values ("
               ^(wrap usrid) ^ ","
               ^ (wrap eid) ^ ","
               ^ (wrap "user") ^ ")");
      add_usr t in
  add_usr usrlist;
  PGOCaml.close db

let change_admin usrid eid right =
  let db = connect () in
  let _ = exec db ("update admin set admin_right = " ^ (wrap right)
                   ^ "where aid = " ^ (wrap usrid) ^ " and "
                   ^ "eid = " ^ (wrap eid)) in
  PGOCaml.close db

let remove_timeslot usrid eid (day, time) =
  let db = connect () in
  let _ = exec db ("delete from availability where aid =" ^ (wrap usrid)
                   ^ " and eid = " ^ (wrap eid)
                   ^ " and day = " ^ (string_of_int day)
                   ^ " and time = " ^ (string_of_int time)) in
  PGOCaml.close db

(* [admin_of_event eventid] is the admin of [eventid]*)
let admin_of_event eventid =
  let db = connect () in
  let result = exec db ("select aid from admin where eid ="
                        ^ (wrap eventid)
                        ^ " and admin_right =" ^ (wrap "admin")) in
  PGOCaml.close db;
  match result with
  | [[Some str]] -> str
  | _ -> raise (DataError)

let finalize eventid =
  let admin_id = admin_of_event eventid in
  let db = connect () in
  let subquery = "(select day, time, count(*) from availability"
                 ^ " where eid = " ^ (wrap eventid)
                 ^ " and aid = " ^ (wrap admin_id)
                 ^ " group by day, time"
                 ^ " order by count DESC)" in
  let query = "select day, time from " ^ subquery ^ " as cnt limit 1" in
  let result = exec db query in
  match result with
  | [Some day::Some time::[]] ->
    let _ = exec db ("update event set final_day = " ^ day
                     ^ ", final_time = " ^ time
                     ^ ", status = " ^ (wrap "finalized")
                     ^ " where eid = " ^ (wrap eventid)) in
    PGOCaml.close db;
    (int_of_string day, int_of_string time)
  | _ -> raise (DataError)

let status_of_event eventid =
  let db = connect () in
  let result = exec db ("select status, final_day, final_time " ^
                        "from event where eid =" ^ (wrap eventid)) in
  PGOCaml.close db;
  match result with
  | [[Some "undecided"; _ ; _ ]] -> Open
  | [[Some "finalized"; Some day; Some time]] ->
    Finalized (int_of_string day, int_of_string time)
  | _ -> raise (DataError)

let finalized_event usrid =
  let db = connect () in
  let result = exec db ("select E.eid from event E, admin A "
                        ^ "where E.status =" ^ (wrap "finalized")
                        ^ " and A.eid = E.eid"
                        ^ " and A.aid = " ^ (wrap usrid)) in
  PGOCaml.close db;
  (List.map (function
     | [(Some eid)] -> eid
     | _ -> raise (DataError)) result)

let quit usrid eventid =
  let db = connect () in
  let _ = exec db ("delete from admin where eid =" ^ (wrap eventid)
                   ^ " and aid =" ^ (wrap usrid)) in
  let _ = exec db ("delete from availability where eid =" ^ (wrap eventid)
                   ^ " and aid =" ^ (wrap usrid)) in
  PGOCaml.close db

let delete_all () =
  let db = connect () in
  let _ = exec db ("delete from account");
    exec db ("delete from admin");
    exec db ("delete from event");
    exec db ("delete from availability") in
  PGOCaml.close db
