with Terminal_Interface.Curses; use Terminal_Interface.Curses;

with Ada.Text_IO;             use Ada.Text_IO;



package  Process_Menu is

   type String_Access is access String;
   type Function_Access is access function return Boolean;
   type Menu_Record is record
      Prompt : String_Access;
      Func :  access procedure;  --Function_Access;
   end record;
   type Menu_Type is array (Positive range <>) of Menu_Record;

   procedure Open_Menu (Function_Number : Column_Position; Menu_Array : Menu_Type; Win : Window := Standard_Window);

end Process_Menu;
