with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

with Ada.Text_IO;             use Ada.Text_IO;


package Display_Warning is

   Cancel : Boolean := False;

   procedure Warning (Message : String; Down : Integer := 0);
   function GetYN (Message : String; Down : Integer := 0) return Boolean;
end Display_Warning;
