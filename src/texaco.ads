with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package Texaco is
   c :Key_Code;
   Ch : Character;
   Current_Char : Column_Position := 1;


   -- procedure Line_Editor (Edline : in out Unbounded_String);
   procedure Line_Editor (win1 : Window;
                          StartLine : Line_Position;
                          StartColumn :Column_Position;
                          EditLength : Column_Position;
                          Edline : in out Unbounded_String;
                          MaxLength : Integer);
end Texaco;