with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Display_Warning; use Display_Warning;
with Texaco;  -- For GetKey input function

package body Process_Menu is

   procedure Open_Menu (Function_Number : Column_Position; Menu_Array : Menu_Type) is
      menu_win : Window;
      Ch :Character;
      Current_Line : Integer := 1;

      c : Key_Code;
      Lines : Line_Position;
      Columns : Column_Position;

      procedure HiLite (Win : Window; Menu_Array : Menu_Type; Item_Num : Integer) is
      begin
         Set_Character_Attributes(Win, (Reverse_Video => True,others => False));
         Add (Win => Win,
              Line => Line_Position(Item_Num),
              Column => 1,
              Str => Menu_Array(Item_Num).Prompt.all);
         Refresh(Win);
      end HiLite;

      procedure LoLite (Win : Window; Menu_Array : Menu_Type; Item_Num : Integer) is
      begin
         Set_Character_Attributes(Win, Normal_Video);
         Add (Win => Win,
              Line => Line_Position(Item_Num),
              Column => 1,
              Str => Menu_Array(Item_Num).Prompt.all);
         Refresh(Win);
      end LoLite;



   begin
      Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);
      menu_win := Sub_Window(Win => Standard_Window,
                             Number_Of_Lines => 10,
                             Number_Of_Columns => 20,
                             First_Line_Position => Lines -12,
                             First_Column_Position => (Function_Number-1)*10);
      Clear(menu_win);
      Box(menu_win);

      for i in Menu_Array'Range loop
         Add (Win => menu_win,
              Line => Line_Position(i),
              Column => 1,
              Str => Menu_Array(i).Prompt.all);
      end loop;
      Refresh(Win => menu_win);

      loop
         HiLite(menu_win,Menu_Array,Current_Line);
         c := Texaco.GetKey; --Get_Keystroke;
         if c in Special_Key_Code'Range then
            case c is
            when Key_Cursor_Down =>
               if (Current_Line < Menu_Array'Last) then
                  LoLite(menu_win,Menu_Array,Current_Line);
                  Current_Line := Current_Line +1;
               end if;
            when Key_Cursor_Up =>
               if (Current_Line > Menu_Array'First) then
                  LoLite(menu_win,Menu_Array,Current_Line);
                  Current_Line := Current_Line -1;
               end if;
            when others => exit;
            end case;
         elsif c in Real_Key_Code'Range then

            Ch := Character'Val (c);

            case Ch is
            when LF | CR =>
               begin
                  Clear(Win => menu_win);
                  Refresh(menu_win);

                  Menu_Array(Current_Line).Func.all;

                  exit;
               end;
            when ESC =>
               begin
                  exit;
               end;
            when others => null;
            end case;
         end if;

      end loop;
      Clear(Win => menu_win);
      Refresh(menu_win);
      Delete (Win => menu_win);
   end Open_Menu;
end Process_Menu;
