with Ada.Text_IO;
with Ada.Directories;
use Ada.Text_IO;
use Ada.Directories;

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Calendar;            use Ada.Calendar;

With Ada.Text_IO.Unbounded_IO;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers; use Ada.Containers;

with Display_Warning;
with Process_Menu;
with Message.Login;



package Message.Reader is
   package SU renames Ada.Strings.Unbounded;
   package SUIO renames Ada.Text_IO.Unbounded_IO;
   package SF renames Ada.Strings.Fixed;

   type Directory_Record is record
      FileName : Unbounded_String;
      Prompt : Unbounded_String;
  --    Func : Function_Access;
   end record;

   package Directory_List is new Ada.Containers.Doubly_Linked_Lists(Directory_Record);
   use Directory_List;
   Directory_Buffer : Directory_List.List;

   CurrentLine : Line_Position := 0;
   CurrentCurs : Cursor;
   TopLine : Line_Position;
   TermLnth : Line_Position;
   TermWdth : Column_Position;
   BottomLine : Line_Position ;

   Curr_Dir : string := Current_Directory;


   procedure Read_Directory (ReplyID : Unbounded_String := To_Unbounded_String(""));

   procedure Read_Messages;

   procedure Read_Header (FileName : in String;
                          Sender : out Unbounded_String;
                          Subject : out Unbounded_String;
                          Msgid : out Unbounded_String;
                          ReplyTo : out Unbounded_String);

  -- function Post_Message  (ReplyID : in Unbounded_String := To_Unbounded_String("");
  --                         ReplySubject : in Unbounded_String := To_Unbounded_String("") )  return Boolean;
   function CharPad(InStr : Unbounded_String; PadWidth : Integer) return String;

end Message.Reader;
