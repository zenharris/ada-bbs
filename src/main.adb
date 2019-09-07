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
with Ada.Text_IO.Unbounded_IO;
-- with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Ada.Directories;
use Ada.Text_IO;
use Ada.Directories;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers; use Ada.Containers;



with Text_File_Scroller;
with Process_Menu;
with Texaco;
with Message.Reader; use Message.Reader;

with Pong_Bot;

with Serpent;

with Formatter;

with Dbase.Scroller;



procedure Main is
   c : Key_Code;
   Lines : Line_Position;
   Columns : Column_Position;

   package SU renames Ada.Strings.Unbounded;
   package SUIO renames Ada.Text_IO.Unbounded_IO;
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

   procedure Serpent_Scoreboard;


   function test_func return Boolean is
   begin
      return True;
   end test_func;


   procedure logout is
   begin
   --   Abort Display_Current_Time;
      End_Windows;
      Curses_Free_All;

      raise PROGRAM_ERROR with "Aborted because User Logout.  Bye";

   end logout;

   procedure Run_Fkey1 is
   begin
      Text_File_Scroller("main.adb");

   end;

   function Run_Pong_Bot return Boolean is
   begin

      Pong_Bot.Irc_Client;
      return True;
   end;

   procedure Run_Line_Editor is
      Edline : Unbounded_String := To_Unbounded_String("");
      c : Key_Code;
   begin

      Texaco.Password_Editor(Standard_Window,
                         StartLine => Lines-3,
                         StartColumn => 0,
                          Edline => Edline,
                         MaxLength => 15);
      Add(Standard_Window,Column => 0,Line => 10,Str => To_String(Edline));
      Refresh;
      c := Get_Keystroke;

   end;



   function Run_Message return Boolean is
   begin
      Message.Reader.Read_Messages;
      -- c := Get_Keystroke;
      return True;
   end Run_Message;


   procedure Run_Serpent is
   begin
      Serpent;
      Serpent_Scoreboard;

   end;

   procedure Read_Score_File (FileName : in String;
                              Nick : out Unbounded_String;
                              Score : out Unbounded_String;
                              Date : out Unbounded_String) is

      HeaderType, HeaderText, scratch : Unbounded_String;
       File : File_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => Filename);

      scratch := SUIO.Get_Line(File);
      while scratch /= "" loop
         HeaderType := To_Unbounded_String(SU.Slice(scratch,1,SU.Index(scratch,":")-1));
         HeaderText := To_Unbounded_String(SU.Slice(scratch,SU.Index(scratch,":")+2,SU.Length(scratch)));
         if HeaderType = "Nick" then
            Nick := HeaderText;
         elsif HeaderType = "Score" then
            Score := HeaderText;
         elsif HeaderType = "Date" then
            Date := HeaderText;
         end if;
         scratch := SUIO.Get_Line(File);
      end loop;
      Close (File);
   exception
      when End_Error =>
         Close (File);
         null;
   end Read_Score_File;


   procedure Serpent_Scoreboard is
      use Message.Reader.Directory_List;
      Curr_Dir : string := Current_Directory;
      Dbuff : Message.Reader.Directory_List.List;
      Dir : Directory_Entry_Type;
      Dir_Search : Search_Type;
      Nick,Score,Date : Unbounded_String;
      I, J, SortCurs : Cursor;
      swapped : Boolean;
      Linenum : Line_Position := 1;
      Display_Window : Window;
      Width,Columns : Column_Position := 46;
      Length,Lines : Line_Position := 20;
      c : Key_Code := 0;
   begin
      Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);
      Display_Window := Sub_Window(Win => Standard_Window,
                                   Number_Of_Lines => Length,
                                   Number_Of_Columns => Width,
                                   First_Line_Position => (Lines - Length) / 2,
                                   First_Column_Position => (Columns - Width) / 2);

      Clear(Display_Window);
      Box(Display_Window);
      Refresh(Display_Window);


      Start_Search(Search => Dir_Search,
                   Directory => Curr_Dir&"/serpent",
                   Pattern => "*.score");

      if More_Entries(Dir_Search) then

         loop
            exit when not More_Entries(Dir_Search);
            Get_Next_Entry(Dir_Search, Dir);
            Read_Score_File(Full_Name(Dir),Nick,Score,Date);
            Dbuff.Append( (To_Unbounded_String(Full_Name(Dir)),
                          Message.Reader.CharPad(Nick,15) &
                            Message.Reader.CharPad(Score,5) & Date ) );

         end loop;
         End_Search(Dir_Search);

         loop
            SortCurs := Dbuff.First;
            swapped := False;
            while SortCurs /= Dbuff.Last loop
               I := SortCurs;
               Directory_List.Next(SortCurs);
               J := SortCurs;
               if Element(J).FileName > Element(I).FileName then
                  Swap(Dbuff,I,J);
                  swapped := True;
               end if;
            end loop;
            exit when not swapped;
         end loop;

         SortCurs := Dbuff.First;
         while SortCurs /= Dbuff.Last loop
            Add (Display_Window,Line => Linenum,Column => 2,Str => To_String(Element(SortCurs).Prompt));
            Linenum := Linenum + 1;
            Directory_List.Next(SortCurs);
            if Linenum = Length -2 then
               Add(Display_Window,Linenum,2,Str => "Any Key More  Esc exit");
               Refresh(Display_Window);
               c := Texaco.GetKey;

               exit when (c in Real_Key_Code'Range) and then (Character'val(c) = ESC);

               Clear(Display_Window);
               Box(Display_Window);
               Linenum := 1;
            end if;

         end loop;
         Add (Display_Window,Line => Linenum,Column => 2,Str => To_String(Element(SortCurs).Prompt));
         Refresh(Display_Window);
      end if;

      if c = 0 or else Character'Val(c) /= ESC then
         c := Texaco.GetKey;
      end if;


      Clear(Display_Window);
      Refresh(Display_Window);
      Delete (Win => Display_Window);

   end Serpent_Scoreboard;


   Menu1 : Process_Menu.Menu_Type  :=
     ((new String'("Message Forum"),Message.Reader.Read_Messages'Unrestricted_Access),
      (new String'("IRC Chat"),Pong_Bot.Irc_Client'Unrestricted_Access),
      (new String'("Serpent Game"),Run_Serpent'Unrestricted_Access),
      (new String'("Serpent Scores"),Serpent_Scoreboard'Unrestricted_Access),
      (new String'("Database"),Dbase.Scroller.Run'Unrestricted_Access),
      (new String'("Log Out"),logout'Unrestricted_Access));


   Visibility : Cursor_Visibility := Invisible;
begin

   -- Dbase.Scroller.test;
   -- raise PROGRAM_ERROR with "Aborted because User Logout.  Bye";

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
