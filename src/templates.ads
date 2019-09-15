with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;


with GNAT.Regpat; use GNAT.Regpat;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with Ada.Containers.Indefinite_Ordered_Maps;

with gnatcoll.SQL.Postgres;  use gnatcoll.SQL.Postgres;
with gnatcoll.SQL.Exec;      use gnatcoll.SQL.Exec;

with Dbase;
with Texaco;

package Templates is
   package SU renames Ada.Strings.Unbounded;
   package SUIO renames Ada.Text_IO.Unbounded_IO;


   package Screen_Vector is new Ada.Containers.Vectors (Natural,
                                                    Unbounded_String);
   use Screen_Vector;

   type Edit_Fields_Record is record
      Name : Unbounded_String;
      Row : Line_Position;
      Col : Column_Position;
      Length : Integer;
   end record;
   package Edit_Fields_Vector is new Ada.Containers.Vectors (Natural,
                                                    Edit_Fields_Record);
   use Edit_Fields_Vector;

    package Current_Record_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Unbounded_String);

   use Current_Record_Maps;

   Current_Record : Map;


   procedure Display_Page (CI : Direct_Cursor);


end Templates;
