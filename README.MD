Instructions to Compile and Run Coodle
===============================================================================
1.Install PostgreSQL (assume Homebrew is already installed and no previous version of PostgreSQL is installed):
  Run install_db.command.
This shell script will install PostgreSQL using Homebrew and set up a local database server.
===============================================================================
2. Install and Run Coodle:
After downloading Coodle folder to computer, open terminal and enter command
cd path_to_WebCoodle/WebCoodle/coodle. For example, if you download Coodle to
Desktop, use command $cd Desktop/WebCoodle/coodle.
Enter $make test.byte to compile WebCoodle and run ocsigenserver on it.
When the terminal finishes compiling and the server is connected, you can see
the “ocsigenserver  -c local/etc/coodle/coodle-test.conf” in the terminal.
You have finished compiling WebCoodle and connecting to the ocsigenserver server.
Open Chrome browser and put your local host address “http://localhost:8080/” in
your address bar.

*Please note that our product is currently only supported by Chrome*

===============================================================================
******You will see the coodle login page******
If you are new to coodle, please click on the “create an account” link and you
will be directed to the sign up page.

******You will see the sign up page******
Please enter your preferred username and password in the textfield and then
click the “Sign up” button.

-If the submitted username has been taken, you will see the error message
“Username has been taken.” Please choose another username and create the
account again.

-If your choice of username/password is valid, you will be directed to
the event page.

-If you want to go back to the login page, click the link “Back” below the
“Sign up” button.

-If you already have a coodle account, input username and password in
corresponding textfield and click the “Log in” button. If you entered the wrong
username/password combination, you will see an error message “Wrong Password”
above the username textfield. If you entered the correct username/password
combination, you will be directed to the event page associated to your account.

*******You will see the event page*******
On the top menu bar, you can click “Log out” to quit and redirect to the login page.
Or you can click “My Schedule” to see your finalized upcoming events or past events.
Or you can click “Create New Event” to be redirected to the new event page.

-If you clicked on “Create New Event”, you will see a form where you enter
the event name, select event start time and select whether you allow Anonymous
Selection that hides other users’ time slot selection from all users. You can
also enter an optional event description. Then click the “submit” button to
submit your new event.

-After successfully submitting a new event, you will be redirected to the
event page. You can see your list of events below the message
“Here are your events”.

-If you click on any event link, you will be directed to the calendar page
of that event in which you can select your available time slots.

*******calendar page********
-If you enter the calendar page for an arbitrary event. You will see:
A menu bar consists of “Home”, “Invite”, “Add Managers” and “Log out”.

-If you click on “Home”, you will be redirected to your event page.

-If you click on “Invite”, you will be redirected to the invite page where
you can invite any user in the existing database. If you enter a user who is
not in the database, you will receive an error message “No such user”. If you
successfully invite a user, you can see the invited users below the textfield.
Then you can click the link “back” to go back to the calendar page.

-If you are the admin of the event, you can click on “Add Managers” to be
redirected to the setting page where you can input a username in the textfield.
Once you click “confirm”, if the username is not invited for this event, you
will see the error message “User Does not exist in current event”. Otherwise,
you will see a message “Success” and all username of managers can be seen below
the textfield. Then you can click the link “back” to go back to the calendar page.

-On the calendar page, you can click on any time slot on the time slot table to
indicate your available time slots for the event. Once you are done selecting,
click on the submit button to submit your available time slot(s).

-On the time slot table, you can see your submitted selections in light pink
time slots with dashed line around the time slot. And you can see other users’
selections in dark pink time slots. If the event does not allow anonymous
selection, then you can hover your cursor on the time slot to see names of
participants and the number of participants for each time slot (appeared below
the “finalize” button).

-If you are the admin of the event, you can click the “finalize” button to finalize
the date and time of the event. Once the event is finalized, you can see the date
and time of this event under “My Schedule” in your event page.

-If you are not the admin of the event, you can click “Quit” button to delete the
event from your event list and also delete yourself from the event.
