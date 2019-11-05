with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;


with GNAT.Regpat; use GNAT.Regpat;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Fixed;

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

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;


generic

package Templates is
   package SU renames Ada.Strings.Unbounded;
   package SUIO renames Ada.Text_IO.Unbounded_IO;
    package SF renames Ada.Strings.Fixed;


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

   DebugMode : Boolean := False;


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

   subtype Value_Type is Long_Long_Float;
   package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions (
                                                                              Value_Type);
      use Value_Functions;

   type Work_Item is record
      Ship_ID :Unbounded_String; -- new Integer; --range 1 .. 100;
   end record;

   package Work_Item_Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces
           (Element_Type => Unbounded_String);

   package Work_Item_Queues is
     new Ada.Containers.Unbounded_Synchronized_Queues
           (Queue_Interfaces => Work_Item_Queue_Interfaces);

   Firing_Queue : Work_Item_Queues.Queue;
   Torpedo_Firing_Queue : Work_Item_Queues.Queue;
   Torpedo_Tube2_Firing_Queue : Work_Item_Queues.Queue;


   procedure Redraw_Page ;
   procedure Edit_Page ;
   procedure Command_Screen;
   procedure Set_Default (Fldnme : String; Default : String);
   procedure Close_Page;
 --  function Initialise (CI :Direct_Cursor; TableName : String) return Boolean;
   function Initialise (CI :Direct_Cursor;
                        TableName : String;
                        NewRecord : Boolean := False;
                        NoWindow : Boolean := False
                       ) return Boolean;
   procedure Inflict_Damage (ShipID : Unbounded_String;
                             DamageX : Integer := 1;
                             WeaponRange : Long_Long_Float := 200.0;
                             Win : Window);
   procedure Fire_Lasers (Ship_ID : Unbounded_String);
   procedure Fire_Torpedo (Ship_ID : Unbounded_String);
   procedure Fire_Tube2_Torpedo (Ship_ID : Unbounded_String);
   procedure Torpedo_Control (ShipID : Unbounded_String; Win : Window);
end Templates;
