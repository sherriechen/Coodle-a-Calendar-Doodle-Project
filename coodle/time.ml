let to_string_hour (date:Unix.tm) =
  let hour = date.tm_hour in
  string_of_int hour ^ ":00"

let to_string_day (date:Unix.tm) =
  let day = date.tm_mday in
  let month = date.tm_mon + 1 in
  let year = date.tm_year + 1900 in
  string_of_int month^ "-"^
  string_of_int day  ^ "-" ^
  string_of_int year

let string_of_relative_hour hour start =
  let diff = float_of_int (hour-1) *. 60. *. 60. in
  let date' = Unix.localtime (diff +. start) in
  to_string_hour date'

let string_of_relative_day day start =
  let diff = float_of_int (day-1) *. 60. *. 60. *. 24. in
  let date' = Unix.localtime (diff +. start) in
  to_string_day date'

let float_of_string_start date =
  Scanf.sscanf date "%04d-%02d-%02dT%02d"
    (fun yyyy mm dd hh->
       fst (Unix.mktime {Unix.tm_sec=0; tm_min=0; tm_hour=hh;
                         tm_mday=dd; tm_mon=mm-1; tm_year=yyyy-1900;
                         tm_wday=0; tm_yday=0; tm_isdst=false}))
