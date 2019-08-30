(* [to_string_timeslot day start] is a string representation of the day
 * [day]-1 days [start].
 * Example: [to_string_timeslot 3 3 [placeholder]]*)
val string_of_relative_day: int -> float -> string

(* [to_string_timeslot hour start] is a string representation of the hour
 * [hour]-1 hours after [start]. Date is ignored. *)
val string_of_relative_hour: int -> float -> string

(*[float_of_string_start start] returns the float representation of time start
 * which is in string . *)
val float_of_string_start: string -> float
