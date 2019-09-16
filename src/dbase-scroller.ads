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

with GNAT.Regpat; use GNAT.Regpat;

with Templates;



package Dbase.Scroller is
   package SU renames Ada.Strings.Unbounded;
   package SUIO renames Ada.Text_IO.Unbounded_IO;
   type Scrl_Record is record
      ID : Integer;
      Prompt : Unbounded_String;
  --    Func : Function_Access;
   end record;

   package Scrl_List is new Ada.Containers.Doubly_Linked_Lists(Scrl_Record);
   use Scrl_List;



   function Fld (CI : Direct_Cursor; FldNme : Unbounded_String) return String;


   procedure Scroll (SQLstatement : String); --; CI : in out Direct_Cursor);
   procedure Run;

end Dbase.Scroller;
