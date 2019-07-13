with Ada.Text_IO;             use Ada.Text_IO;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
With Ada.Text_IO.Unbounded_IO;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

procedure Text_File_Scroller (FileName : String) is
   File : File_Type;
   Line_Counter : Line_Position := 3;
   Current_Line : Integer := 0;
   c : Key_Code;
   Lines : Line_Position;
   Columns : Column_Position;

   package My_Vector is new Ada.Containers.Vectors (Natural,
                                                    Unbounded_String);
   use My_Vector;
   package SUIO renames Ada.Text_IO.Unbounded_IO;

   str_pool : My_Vector.Vector;

   procedure read_input (str_pool : in out My_Vector.Vector;
                         File     : File_Type) is
   begin
      loop
         str_pool.Append (SUIO.Get_Line (File));
      end loop;
   exception
      when End_Error =>
         null;
   end read_input;

   procedure Redraw_List is
      Line_Cursor : Line_Position := 3;
      Line_Iter : Integer := 0;
   begin
      Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);

      loop
         if (Line_Cursor >= Lines - 3) then
            exit;
         end if;
         if (Current_Line+Line_Iter > Integer(str_pool.Length)-1) then
            Clear_To_End_Of_Screen;
            Refresh;
            exit;
         end if;
         Move_Cursor (Standard_Window,Column => 2,Line => Line_Cursor);
         Clear_To_End_Of_Line;
         Add (Win => Standard_Window,
              Column => 2,
              Line => Line_Cursor,
              Str => To_String(str_pool.Element(Current_Line+Line_Iter)));

         Line_Iter := Line_Iter + 1;
         Line_Cursor := Line_Cursor + 1;
      end loop;
      Refresh;
   end Redraw_List;


begin
   Clear;
   Refresh;

   Open (File => File,
         Mode => In_File,
         Name => FileName);

   While not  End_Of_File (File) Loop
      str_pool.Append (SUIO.Get_Line (File));
   end loop;
   --read_input (str_pool, File);
   Close (File);

   Current_line := 0;
   Redraw_List;
   Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);

   loop
      -- Clear;
      Redraw_List;
      Add (Line => Lines - 2,Column => 1, Str => " Page Up/Down Arrow Up/Down                                Esc exit");
      c := Get_Keystroke;
      if c in Special_Key_Code'Range then
         case c is
            when Key_Cursor_Down =>
               if Current_Line < Integer(str_pool.Length)-1 then
                  Current_Line := Current_Line + 1;
               end if;
            when Key_Cursor_Up =>
               if Current_Line > 0 then
                  Current_Line := Current_Line -1;
               end if;
            when Key_Next_Page =>
               if Current_Line + (Integer(Lines)-6) < Integer(str_pool.Length) then
                  Current_Line := Current_Line + (Integer(Lines)-6);
               end if;
            when Key_Previous_Page =>
               if Current_Line - (Integer(Lines)-6) > 0  then
                  Current_Line := Current_Line - Integer(Lines);
               else
                  Current_Line := 0;
               end if;
            when Key_Home =>
               Current_Line := 0;

            when Key_End =>
               Current_Line := Integer(str_pool.Length)-1;
               -- exit;
               when others => null;
         end case;
      elsif c in Real_Key_Code'Range then

         case Character'Val (c) is
            when ESC =>
               begin
                  exit;
               end;
            when others => null;
         end case;


      end if;
   end loop;

   Clear;
   Refresh;
   str_pool.Clear;

end Text_File_Scroller;
