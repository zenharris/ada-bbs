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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with Dbase;
with Texaco;
with Process_Menu;
with Formatter;
-- with Dbase.DrackSpace;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.discrete_Random;

generic

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
      Edited : Boolean := False;
      NoEdit : Boolean := False;
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
   Current_Record_Updated : Boolean := False;


   ScreenList : Screen_Vector.Vector;
   FieldsList : Screen_Vector.Vector;
   EditFieldsList : Edit_Fields_Vector.Vector;
   Display_Window : Window;
   SaveTableName : Unbounded_String;


   type Days_of_Week is (Sunday,
                         Monday,
                         Tuesday,
                         Wednesday,
                         Thursday,
                         Friday,
                         Saturday);
   package Ada_Format is
     new Formatter (Enumerated => Days_of_Week);
   use     Ada_Format; -- Direct visibility of F conversion functions

   procedure Redraw_Page ;
   procedure Edit_Page ;
   procedure Command_Screen;
   procedure Set_Default (Fldnme : String; Default : String);
   procedure Close_Page;
 --  function Initialise (CI :Direct_Cursor; TableName : String) return Boolean;
   function Initialise (CI :Direct_Cursor; TableName : String;NewRecord : Boolean := False) return Boolean;
   procedure Inflict_Damage (ShipID : Unbounded_String);

end Templates;
