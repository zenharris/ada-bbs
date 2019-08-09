
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
With Ada.Text_IO.Unbounded_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;

with Texaco; use Texaco;
with Display_Warning;


package Message.Post is
   package SU renames Ada.Strings.Unbounded;

   procedure Post_Message (ReplyID : in Unbounded_String := To_Unbounded_String("");
                          ReplySubject : in Unbounded_String := To_Unbounded_String(""));
end Message.Post;
