
package body Texaco is
  --   c :Key_Code;
  --    Ch : Character;
  --    Current_Char : Column_Position := 1;

   procedure Line_Editor (win1 : Window;
                          StartLine : Line_Position;
                          StartColumn :Column_Position;
                          EditLength : Column_Position;
                          Edline : in out Unbounded_String;
                          MaxLength : Integer) is

    --  Lines : Line_Position;
      Columns : Column_Position := StartColumn+EditLength;
      ScreenOffset, endpoint : Integer := 0;

   begin
    --  Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);

      Move_Cursor(Win => win1,Line => StartLine,Column => StartColumn);
      Clear_To_End_Of_Line(win1);
      Refresh(win1);
      Current_Char := 1;

      loop
         Move_Cursor(win1,Line   => StartLine,Column => StartColumn);
         Clear_To_End_Of_Line(win1);

         if Length(Edline) > ScreenOffset + Integer(Columns-StartColumn) then
            endpoint := ScreenOffset + Integer(Columns-StartColumn);
         else
            endpoint := Length(Edline);
         end if;
         Add (win1,Column => StartColumn,Line => StartLine,
              Str => Slice(Edline,ScreenOffset+1,endpoint));

        -- Refresh(win1);

         Move_Cursor(Win => win1,
                     Line => StartLine,
                     Column => StartColumn + (Current_Char-Column_Position(ScreenOffset)) -1);

         Refresh(win1);
         c := Get_Keystroke;
         if c in Special_Key_Code'Range then
            case c is
            when Key_Backspace =>

               if Current_Char > 1 then
                  Current_Char := Current_Char -1;

                  Ada.Strings.Unbounded.Delete(Source => Edline,
                                               From => Integer(Current_Char),
                                               Through => Integer(Current_Char));

               end if;
               if Integer(Current_Char)-ScreenOffset = 0 then
                  if ScreenOffset > Integer(Columns-StartColumn) -1 then
                     ScreenOffset := ScreenOffset -(Integer(Columns-StartColumn) -1);
                  else
                     ScreenOffset := 0;
                  end if;
               end if;

               when Key_Delete_Char =>

                  if Integer(Current_Char) <= Length(Edline) then
                     Ada.Strings.Unbounded.Delete(Source => Edline,
                                                  From => Integer(Current_Char),
                                                  Through => Integer(Current_Char));
                  end if;


               when Key_Cursor_Left =>
                  if Current_Char > 1 then
                     Current_Char := Current_Char - 1;
                  end if;
                  if Integer(Current_Char)-ScreenOffset = 0 then
                     if ScreenOffset > Integer(Columns-StartColumn) -1 then
                        ScreenOffset := ScreenOffset -(Integer(Columns-StartColumn) -1);
                     else
                        ScreenOffset := 0;
                     end if;

                  end if;

               when Key_Cursor_Right =>
                  if Integer(Current_Char) <= Length(Edline) then
                     Current_Char := Current_Char + 1;
                  end if;
                  if Integer(Current_Char)-ScreenOffset = Integer(Columns-StartColumn)+1 and then Length(Edline) < MaxLength then
                     ScreenOffset := ScreenOffset +(Integer(Columns-StartColumn) -1);
                  end if;
               when Key_F1 | Key_F2 | Key_F3 | Key_F4 | Key_F5 | Key_F6 | Key_F7 | Key_F8 => exit;


               when others => null;
            end case;
         elsif c in Real_Key_Code'Range then

            Ch := Character'Val (c);

            case Ch is

            when CR | LF => exit;

               when ESC => exit;

               when others =>
               if Length(Edline) < MaxLength then
                  Add (win1,Ch => Ch,
                       Line => StartLine,
                       Column => StartColumn + Current_Char-Column_Position(ScreenOffset)-1 );
                  Refresh(win1);

                  Ada.Strings.Unbounded.Insert (Source => Edline,
                                                Before => Integer(Current_Char),
                                                New_Item => ("" & Ch));

                  Current_Char := Current_Char +1;

                  if Integer(Current_Char)-ScreenOffset = Integer(Columns-StartColumn)+1 and then Length(Edline) < MaxLength then
                     ScreenOffset := ScreenOffset + (Integer(Columns-StartColumn)-1);
                  end if;

               end if;

            end case;
         end if;
      end loop;

   end Line_Editor;

end Texaco;
