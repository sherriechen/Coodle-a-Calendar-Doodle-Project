(* admin_right represents a user's right within an event*)
type admin_right = Admin | Manager | User

(* [event_status] represents the status of an event. [Open] means it is
undecided and open to add timeslots; [Finalized (day, time)] means it's time
has been decided to be (day, time) *)
type status = Open | Finalized of (int * int)

(* [event_desc eventid] is the description of event [eventid]; if such event
 * does not have a description return "no description" *)
(* requires: [eventid] exists in database *)
val event_desc : string -> string

(* [events_of_user usrid] is a id list of events that a user has participated
 * in.*)
(* requires: [usrid] exists in database *)
val events_of_user : string -> string list

(* [users_of_event eventid] is a id list of users that have participated in
 * event with [eventid] *)
(* requires: [eventid] exists in database *)
val users_of_event : string -> string list

(* [user_event_right eventid userid] is the admin right of a user with [userid]
 * to the event with [eventid]*)
(* requires: [userid], [eventid] exists in database and [userid] has
   participated in [eventid]*)
val user_event_right : string -> string -> admin_right

(* [add_new_account name pwd] create account with username [name]
 * and passward [pwd] in the database.
 * requires: [username_unique name] is ture*)
val add_new_account : string -> string -> unit

(* [check_password name pwd] is true if username [name] is linked with password
 * [pwd] in the database, false otherwise. If username [name] does not exist,
 * it is false. *)
val check_password  : string -> string -> bool

(* [check_user_valid usrid] is true if user [usrid] exists in database otherwise
 * false *)
val check_user_valid : string -> bool

(* [check_event_valid eventid] is true if event [eventid] exists in
 * database otherwise false *)
val check_event_valid : string -> bool

(* [if_admin usrid eventid] is true if [usrid] is the admin of [eventid]*)
val if_admin : string -> string -> bool

(* [if_manager usrid eventid] is true if [usrid] is the manager of [eventid]*)
val if_manager : string -> string -> bool

(* [username_unique name] is false if there exists user name [name] in database,
 * true otherwise.*)
val username_unique  : string -> bool

(* [eventname_unique name] is false if there exists event name [name] in database,
 * true otherwise.*)
val eventname_unique  : string -> bool

(* [all_events usrid]* is a id list of all the events of the user [usrid]*)
val all_events : string -> string list

(* [timeslots_of_event eventid] is a list of timeslot information in the form
 * (user, time) ex. ("user","2435340.44")*)
val timeslots_of_event : string -> (string * int * int) list

(* [starttime_of_event eventid] is the float representation of the start time
 * of event named [eventid]*)
val starttime_of_event : string -> float

(* [add_new_event usrid eventid desc invite start_time is_anonymous] create a
   new event with [usrid] as creator, [eventid] as id, [desc] as description,
   [invite] as invite list, [start_time] as start time represented by a float.
   If [is_anonymous] is true, then all its participants' names are displayed
   as "Anonymous" *)
val add_new_event : string -> string -> string -> string list -> float -> bool -> unit

(* [add_timeslot usrid eventid slots] add a list of timeslots for user with id
   [usrid] and event with [eventid]. [slots] is a list of time represented by
   tuple (day * time) *)
val add_timeslot : string -> string -> (int * int) list -> unit

(* [remove_timeslot usrid eventid day time] removes the timeslot of [usrid],
   event [eventid] on [day] [time] *)
val remove_timeslot : string -> string -> (int * int) -> unit

(* [invite eventid usrlist] add a list of users to event [eventid] *)
val invite : string -> string list -> unit

(* [change_admin usrid eventid newright] changes [usrid]'s admin_right on
   [eventid] to [newright]*)
val change_admin : string -> string -> string -> unit

(* [user_on_slot_of_event (day, time) eventid] is the list of users who have
   chosen the timeslot [(day, time)]*)
val user_on_slot_of_event : (int * int) -> string -> string list

(* [managers_of_event eventid] is a list of users who are the managers of
   event [eventid]*)
val managers_of_event : string -> string list

(* [finalize eventid] is the best timeslot for the event [evnetid], which
is the time slot with the most people available *)
val finalize : string -> (int * int)

(* [status_of_event eventid] is the status of [eventid] *)
val status_of_event : string -> status

(* [finalized_event usrid] is the list of finalized events of user [usrid] *)
val finalized_event : string -> string list

(* [quit usrid eventid] deletes [usrid] from [eventid] *)
val quit : string -> string -> unit

(* [delete_all ()] deletes all entries in all tables*)
val delete_all : unit -> unit
