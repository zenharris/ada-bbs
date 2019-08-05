with Terminal_Interface.Curses; use Terminal_Interface.Curses;

with Ada.Text_IO;             use Ada.Text_IO;



package body Display_Warning is

   procedure Warning (Message : String) is
      Display_Window : Window;
      Width : Column_Position := 40;
      Length : Line_Position := 5;
      c : Key_Code;
      Lines : Line_Position;
      Columns : Column_Position;

   begin
      Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);

      Display_Window := Sub_Window(Win => Standard_Window,
                                   Number_Of_Lines => Length,
                                   Number_Of_Columns => Width,
                                   First_Line_Position => (Lines - Length) / 2,
                                   First_Column_Position => (Columns - Width) / 2);

      Clear(Display_Window);
      Box(Display_Window);

      Add (Win => Display_Window,
           Column => Column_Position((Width - Message'Length) / 2),
           Line => 2,
           Str => Message);

      Add (Win => Display_Window,Column => 1,Line => 4,Str => "Any Key to Continue");
      Refresh(Display_Window);
      c := Get_Keystroke;
      Clear(Display_Window);
      Refresh(Display_Window);
      Delete (Win => Display_Window);
   end;

   function GetYN (Message : String) return Boolean is
      Display_Window : Window;
      Width : Column_Position := 40;
      Length : Line_Position := 5;
      c : Key_Code;
      Lines : Line_Position;
      Columns : Column_Position;
      retval : Boolean;
   begin
      Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);

      Display_Window := Sub_Window(Win => Standard_Window,
                                   Number_Of_Lines => Length,
                                   Number_Of_Columns => Width,
                                   First_Line_Position => (Lines - Length) / 2,
                                   First_Column_Position => (Columns - Width) / 2);

      Clear(Display_Window);
      Box(Display_Window);

      Add (Win => Display_Window,
           Column => 1,
           Line => 2,
           Str => Message);


      Add (Win => Display_Window,Column => 1,Line => 4,Str => "y/n to Continue");
      Refresh(Display_Window);
      loop
         c := Get_Keystroke;
         case Character'Val (c) is
            when 'Y'|'y' =>
               retval := True;
               exit;
            when 'N'|'n' =>
               retval := False;
               exit;
            when others => null;
         end case;
      end loop;



      Clear(Display_Window);
      Refresh(Display_Window);
      Delete (Win => Display_Window);
      return retval;
   end;

end Display_Warning;
