with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Text_IO;             use Ada.Text_IO;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
--with Ada.Characters.Handling; use Ada.Characters.Handling;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- with Ada.Containers.Doubly_Linked_Lists;
with Text_File_Scroller;
with Process_Menu;
with Texaco;

with Pong_Bot;

with Serpent;


procedure Main is
   c : Key_Code;
   Lines : Line_Position;
   Columns : Column_Position;

 --  task Display_Current_Time is
 --     entry Start;
 --  end Display_Current_Time;

 --  task body Display_Current_Time is
 --     Next : Time;
 --     D    : Duration := 1.0;
 --     Now : Time := Clock;
 --     win1 : Window;
 --     Col : Column_Position;
 --     Lin  : Line_Position;
 --  begin
 --     accept Start;
 --     win1 := Sub_Window(Win => Standard_Window,
 --                        Number_Of_Lines => 1,
 --                        Number_Of_Columns => 32,
 --                        First_Line_Position => 1,
 --                        First_Column_Position => 48);
 --     -- Box(win1);
 --     Refresh(Win => win1);
 --     loop
 --        Now := Clock;
 --
 --        Next := Now + D;
 --        Get_Cursor_Position(Line => Lin,Column => Col);
 --        Add (Win => win1,Line => 0,Column => 0,Str => " Sys Time " & Image (Now));
 --        Refresh(Win => win1);
 --        Move_Cursor(Line => Lin,Column => Col);
 --        Refresh;
 --        delay until Next;
 --     end loop;
 --  end Display_Current_Time;



   function test_func return Boolean is
   begin
      return True;
   end test_func;


   function logout return Boolean is
   begin
   --   Abort Display_Current_Time;
      End_Windows;
      Curses_Free_All;

      raise PROGRAM_ERROR with "Aborted because User Logout.  Bye";
      return True;
   end logout;

   function Run_Fkey1 return Boolean is
   begin
      Text_File_Scroller("main.adb");
      return(True);
   end;

   function Run_Pong_Bot return Boolean is
   begin

      Pong_Bot.Irc_Client;
      return True;
   end;

   function Run_Line_Editor return Boolean is
      Edline : Unbounded_String := To_Unbounded_String("");
      c : Key_Code;
   begin

      Texaco.Line_Editor(Standard_Window,
                         StartLine => Lines-3,
                         StartColumn => 0,
                         EditLength => Columns,
                         Edline => Edline,
                         MaxLength => Integer((Columns-1)*5)-2);
      Add(Standard_Window,Column => 0,Line => 10,Str => To_String(Edline));
      Refresh;
      c := Get_Keystroke;
      return True;
   end;

   function Run_Text_Editor return Boolean is

   begin

             Texaco.Text_Editor(Standard_Window,TopLine => 5,BottomLine => Lines-4,MaxLines => 100);

             Clear;
             Refresh;

             Texaco.Dump_List;


      c := Get_Keystroke;
      return True;
   end;


   function Run_Serpent return Boolean is
   begin
      Serpent;

      return True;
   end;

   Menu1 : Process_Menu.Menu_Type  :=
      ((new String'("View Source"),Run_Fkey1'Unrestricted_Access),
      (new String'("IRC Chat"),Run_Pong_Bot'Unrestricted_Access),
      (new String'("Line Editor"),Run_Line_Editor'Unrestricted_Access),
      (new String'("Serpent Game"),Run_Serpent'Unrestricted_Access),
      (new String'("Text Editor"),Run_Text_Editor'Unrestricted_Access),
      (new String'("Platypi"),test_func'Unrestricted_Access),
      (new String'("Log Out"),logout'Unrestricted_Access));


   Visibility : Cursor_Visibility := Invisible;
begin

   Init_Screen;
   Set_Echo_Mode (False);
   Set_Raw_Mode;
   -- Set_Meta_Mode;
   Set_KeyPad_Mode;
   -- Set_Cursor_Visibility(Visibility);



   Nap_Milli_Seconds(200);
   Clear;
   Box;
   Refresh;


  -- Display_Current_Time.Start;

   Move_Cursor (Line => 1, Column => 10);
   Add (Str => " This Is a Test  ");
   Refresh;

   loop



      Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);
      Clear;
      Box;
      Add (Line => Lines - 2,Column => 1, Str => " Func 1  |  Func 2 |  Func 3 |  Func 4 |");
      Refresh;

      c := Texaco.GetKey;  -- Get_Keystroke;
      if c in Special_Key_Code'Range then
         case c is
         when Key_F1 =>
            Process_Menu.Open_Menu (Function_Number => 1,Menu_Array => Menu1 );
         when Key_F2 =>
            Process_Menu.Open_Menu (Function_Number => 2,Menu_Array => Menu1);
         when Key_F3 =>
            Process_Menu.Open_Menu (Function_Number => 3,Menu_Array => Menu1);
         when Key_F4 =>
            Process_Menu.Open_Menu (Function_Number => 4,Menu_Array => Menu1);
         when Key_F10 =>
            Redraw(Win => Standard_Window,Begin_Line => 0,Line_Count => Integer(Lines));
         when Key_End =>
            exit;
            when others => null;
         end case;

      end if;


   end loop;

   -- Abort Display_Current_Time;
   End_Windows;
   Curses_Free_All;


end Main;
