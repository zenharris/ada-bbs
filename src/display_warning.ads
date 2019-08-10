with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

with Ada.Text_IO;             use Ada.Text_IO;


package Display_Warning is

   Cancel : Boolean := False;

   procedure Warning (Message : String);
   function GetYN (Message : String) return Boolean;
end Display_Warning;
