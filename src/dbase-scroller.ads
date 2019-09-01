with gnatcoll.SQL.Postgres;  use gnatcoll.SQL.Postgres;
with gnatcoll.SQL.Exec;      use gnatcoll.SQL.Exec;
with Ada.Text_IO;            use Ada.Text_IO;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Formatter;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers; use Ada.Containers;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;





package Dbase.Scroller is

  type Scrl_Record is record
      ID : Integer;
      Prompt : Unbounded_String;
  --    Func : Function_Access;
   end record;

   package Scrl_List is new Ada.Containers.Doubly_Linked_Lists(Scrl_Record);
   use Scrl_List;
   Scrl_Buffer : Scrl_List.List;

   CurrentLine : Line_Position := 0;
   CurrentCurs : Cursor;
   TopLine : Line_Position;
   TermLnth : Line_Position;
   TermWdth : Column_Position;
   BottomLine : Line_Position ;



    procedure Scroll;

end Dbase.Scroller;
