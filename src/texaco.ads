with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers.Doubly_Linked_Lists;

with Display_Warning;


package Texaco is
   c :Key_Code;
   Ch : Character;
   Current_Char : Column_Position := 1;

   -- package DLL renames Ada.Containers.Doubly_Linked_Lists;
   package SU renames Ada.Strings.Unbounded;

   package String_List is new Ada.Containers.Doubly_Linked_Lists(Unbounded_String);
   use String_List;
   Text_Buffer : String_List.List;

   function GetKey (win1 : Window := Standard_Window) return Key_Code;

   -- procedure Line_Editor (Edline : in out Unbounded_String);
   procedure Line_Editor (win1 : Window;
                          StartLine : Line_Position;
                          StartColumn :Column_Position;
                          EditLength : Column_Position;
                          Edline : in out Unbounded_String;
                          MaxLength : Integer;
                          TextEditMode : Boolean := False;
                         SuppressSpaces : Boolean := False);

   procedure Text_Editor (win1 : Window;
                          TopLine : Line_Position;
                          BottomLine :Line_Position;
                          MaxLines : Integer);
   procedure Dump_List;

   procedure Password_Editor (Win1 : Window;
                              StartLine : Line_Position;
                              StartColumn :Column_Position;
                              Edline : in out Unbounded_String;
                              MaxLength : Integer);
end Texaco;
