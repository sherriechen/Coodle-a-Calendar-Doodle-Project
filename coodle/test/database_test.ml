open OUnit2
open Database

let tests =
[
  "add account1" >:: (fun _ -> assert_equal () (add_new_account "u1" "aaa"));
  "add account2" >:: (fun _ -> assert_equal () (add_new_account "u2" "bbb"));
  "add account3" >:: (fun _ -> assert_equal () (add_new_account "u3" "ccc"));
  "add event" >:: (fun _ -> assert_equal ()
     (add_new_event "u1" "e1" "event1" ["u2";"u3"] 324324.344 true));
  "event_desc1" >:: (fun _ -> assert_equal "event1" (event_desc "e1"));
  "add timeslot"  >:: (fun _ -> assert_equal ()
     (add_timeslot "u1" "e1" [(12,8)]));
  "user_on_slot_of_event" >:: (fun _ -> assert_equal ["u1"]
                              (user_on_slot_of_event (12,8) "e1"));
  "timeslots of event" >:: (fun _ -> assert_equal [("Anonymous",12,8)]
                                     (timeslots_of_event "e1"));
  "check remove timeslot" >:: (fun _ -> assert_equal ()
     (remove_timeslot "u1" "e1" (12, 8)));
  "events of user" >:: (fun _ -> assert_equal ["e1"]
     (events_of_user "u1"));
  "users of event" >:: (fun _ -> assert_equal ["u1";"u2";"u3"]
     (users_of_event "e1"));
  "user_event_right1" >:: (fun _ -> assert_equal Admin
        (user_event_right "e1" "u1"));
  "user_event_right2" >:: (fun _ -> assert_equal User
        (user_event_right "e1" "u2"));
  "username_unique1" >:: (fun _ -> assert_equal true (username_unique "u4"));
  "username_unique2" >:: (fun _ -> assert_equal false (username_unique "u2"));
  "eventname_unique1" >:: (fun _ -> assert_equal false (eventname_unique "e1"));
  "eventname_unique2" >:: (fun _ -> assert_equal true (eventname_unique "e2"));
  "check_password1" >:: (fun _ -> assert_equal true (check_password "u2" "bbb"));
  "check_password2" >:: (fun _ -> assert_equal false (check_password "u2" "bb"));
  "check_password3" >:: (fun _ -> assert_equal false (check_password "u5" "bbb"));
  "all_events1" >:: (fun _ -> assert_equal ["e1"] (all_events "u1"));
  "all_events2" >:: (fun _ -> assert_equal ["e1"] (all_events "u2"));
  "add account4" >:: (fun _ -> assert_equal () (add_new_account "u4" "ddd"));
  "add account5" >:: (fun _ -> assert_equal () (add_new_account "u5" "eee"));
  "invite" >:: (fun _ -> assert_equal () (invite "e1" ["u4";"u5"]));
  "events of user2" >:: (fun _ -> assert_equal ["e1"] (events_of_user "u4"));
  "check_user_valid1" >:: (fun _ -> assert_equal true (check_user_valid "u1"));
  "check_user_valid2" >:: (fun _ -> assert_equal false (check_user_valid "u6"));
  "check_event_valid1" >:: (fun _ -> assert_equal true (check_event_valid "e1"));
  "check_event_valid2" >:: (fun _ -> assert_equal false (check_event_valid "u6"));
  "change_admin" >:: (fun _ -> assert_equal () (change_admin "u2" "e1" "manager"));
  "check admin1" >:: (fun _ -> assert_equal true (if_admin "u1" "e1"));
  "check admin2" >:: (fun _ -> assert_equal false (if_admin "u2" "e1"));
  "start time" >:: (fun _ -> assert_equal 324324.344 (starttime_of_event "e1"));
  "add event2" >:: (fun _ -> assert_equal ()
                   (add_new_event "u2" "e2" "event2" ["u1";"u3"] 34.344 false));
  "event_desc2" >:: (fun _ -> assert_equal "event2" (event_desc "e2"));
  "start time2" >:: (fun _ -> assert_equal 34.344 (starttime_of_event "e2"));
  "check user right3" >:: (fun _ -> assert_equal Manager
                                               (user_event_right "e1" "u2"));
  "managers_of_event" >:: (fun _ -> assert_equal ["u2"]
                                    (managers_of_event "e1"));
  "add event3" >:: (fun _ -> assert_equal ()
         (add_new_event "u1" "e3" "event3" ["u2";"u3";"u4";"u5"] 324.43 false));
  "add timeslot2" >:: (fun _ -> assert_equal () (add_timeslot "u1" "e3" [(1,2)]));
  "timeslots of event2" >:: (fun _ -> assert_equal [("u1",1,2)]
                            (timeslots_of_event "e3"));
  "add timeslot3" >:: (fun _ -> assert_equal () (add_timeslot "u2" "e3" [(1,2)]));
  "add timeslot4" >:: (fun _ -> assert_equal () (add_timeslot "u3" "e3" [(1,2)]));
  "add timeslot4" >:: (fun _ -> assert_equal () (add_timeslot "u4" "e3" [(1,3)]));
  "add timeslot5" >:: (fun _ -> assert_equal () (add_timeslot "u5" "e3" [(1,3)]));
  "add timeslot6" >:: (fun _ -> assert_equal () (add_timeslot "u6" "e3" [(1,4)]));
  "finalize" >:: (fun _ -> assert_equal (1,2) (finalize "e3"));
  "status_of_event1" >:: (fun _ -> assert_equal Open
                        (status_of_event "e1"));
  "status_of_event2" >:: (fun _ -> assert_equal (Finalized (1,2))
                         (status_of_event "e3"));
  "finalized_event" >:: (fun _ -> assert_equal ["e3"] (finalized_event "u1"));
  "add event4" >:: (fun _ -> assert_equal ()
         (add_new_event "u1" "e4" "event4" ["u2";"u3"] 1.43 false));
  "add timeslot1" >:: (fun _ -> assert_equal () (add_timeslot "u2" "e4" [(1,2)]));
  "user_event_right4" >:: (fun _ -> assert_equal User
       (user_event_right "e4" "u2"));
  "quit1" >:: (fun _ -> assert_equal () (quit "u2" "e4"));
  "timeslots of event3" >:: (fun _ -> assert_equal [] (timeslots_of_event "e4"));
]

let suite =
  "database test suite"
  >::: tests

let _ = run_test_tt_main suite
(* ; delete_all () *)
