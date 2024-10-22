
package body Texaco is
  --   c :Key_Code;
  --    Ch : Character;
  --    Current_Char : Column_Position := 1;

   function GetKey (win1 : Window := Standard_Window) return Key_Code is
      Inkey : Key_Code;
   begin
      Inkey := Get_Keystroke(win1);
      return (Inkey);
   exception
      when CONSTRAINT_ERROR =>
         return (c);
   end GetKey;

   procedure Password_Editor (Win1 : Window;
                              StartLine : Line_Position;
                              StartColumn :Column_Position;
                              Edline : in out Unbounded_String;
                              MaxLength : Integer) is
      Current_Char : Column_Position := 1;
      c :Key_Code;
      Ch : Character;
   begin
      Move_Cursor(Win => win1,Line => StartLine,Column => StartColumn);
      Clear_To_End_Of_Line(win1);
      Refresh(win1);


      loop

         Move_Cursor(Win => win1,
                     Line => StartLine,
                     Column => StartColumn + Current_Char -1);

         Refresh(win1);
         c := GetKey;
         if c in Special_Key_Code'Range then
            case c is
            when Key_Backspace =>

               if Current_Char > 1 then
                  Current_Char := Current_Char -1;

                  Add (win1,Ch => ' ',
                          Line => StartLine,
                          Column => StartColumn + Current_Char -1 );


                  Ada.Strings.Unbounded.Delete(Source => Edline,
                                               From => Integer(Current_Char),
                                               Through => Integer(Current_Char));
               end if;

            when others => null;
            end case;
          elsif c in Real_Key_Code'Range then
            Ch := Character'Val (c);
            case Ch is
            when CR | LF => exit;
            when ESC => exit;
            when DEL =>

               if Current_Char > 1 then
                  Current_Char := Current_Char -1;

                  Add (win1,Ch => ' ',
                          Line => StartLine,
                          Column => StartColumn + Current_Char -1 );


                  Ada.Strings.Unbounded.Delete(Source => Edline,
                                               From => Integer(Current_Char),
                                               Through => Integer(Current_Char));
               end if;


            when others =>
               if Length(Edline) < MaxLength then
                  if Ch /= ' ' then

                     Add (win1,Ch => '*',
                          Line => StartLine,
                          Column => StartColumn + Current_Char -1 );
                     Refresh(win1);
                     Ada.Strings.Unbounded.Insert (Source => Edline,
                                                   Before => Integer(Current_Char),
                                                   New_Item => ("" & Ch));

                     Current_Char := Current_Char +1;
                  end if;

               end if;
            end case;
         end if;
      end loop;


   end Password_Editor;



   procedure Line_Editor (win1 : Window;
                          StartLine : Line_Position;
                          StartColumn :Column_Position;
                          EditLength : Column_Position;  -- make this an integer
                          Edline : in out Unbounded_String;
                          MaxLength : Integer;
                          TextEditMode : Boolean := False;
                          SuppressSpaces : Boolean := False;
                          Number :Boolean := False
                         ) is

    --  Lines : Line_Position;
      Columns : Column_Position := StartColumn+EditLength;
      ScreenOffset, endpoint : Integer := 0;


      procedure Clear_Field is
         padstr : Unbounded_String;
      begin
         for i in 1..EditLength loop
               padstr := padstr & " ";
         end loop;

         Move_Cursor(Win => win1,Line => StartLine,Column => StartColumn);
         Add (win1,Column => StartColumn,Line => StartLine,
              Str => To_String(padstr));
         Move_Cursor(Win => win1,Line => StartLine,Column => StartColumn);

      end Clear_Field;



   begin
    --  Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);

      Clear_Field;

      Refresh(win1);

      if TextEditMode = False then
         Current_Char := 1;
      end if;


      loop
         Clear_Field;

         if Length(Edline) > ScreenOffset + Integer(Columns-StartColumn) then
            endpoint := ScreenOffset + Integer(Columns-StartColumn);
         else
            endpoint := Length(Edline);
         end if;
         if endpoint > 0 then
            Add (win1,Column => StartColumn,Line => StartLine,
                 Str => Slice(Edline,ScreenOffset+1,endpoint));
         end if;


        -- Refresh(win1);

         Move_Cursor(Win => win1,
                     Line => StartLine,
                     Column => StartColumn + (Current_Char-Column_Position(ScreenOffset)) -1);

         Refresh(win1);


         c := GetKey;


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


                  if Integer(Current_Char)-ScreenOffset = Integer(Columns-StartColumn)+1 then
                     ScreenOffset := ScreenOffset +(Integer(Columns-StartColumn) -1);
                  end if;

               when Key_Cursor_Up | Key_Cursor_Down | Key_Shift_Delete_Char | Key_F1 | Key_F2 | Key_F3 | Key_F4 | Key_F5 | Key_F6 | Key_F7 | Key_F8 => exit;


               when others => exit; --null;
            end case;
         elsif c in Real_Key_Code'Range then

            Ch := Character'Val (c);

            case Ch is

            when CR | LF => exit;

               when DEL =>

               -- This is a copy of the Backspace code from above
               -- ncurses has decided Backspace is not Special_Key_Code key anymore
               -- and should rather return DEL

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

               when Apostrophe => null;

               when HT => exit;
               when ESC => exit;

               when others =>
                  if Length(Edline) < MaxLength  then
                     if (not Number) or else
                       (Number and then
                          (Ch in '0'..'9' or else (Ch = Full_Stop and then SU.Index(Edline,".") = 0) or else
                               (Current_Char=1 and then
                                    (Ch = Minus_Sign and then SU.Index(Edline,"-") /= 1)))) then

                        if (not SuppressSpaces) or else (SuppressSpaces and then Ch /= ' ') then
                           Add (win1,Ch => Ch,
                                Line => StartLine,
                                Column => StartColumn + Current_Char-Column_Position(ScreenOffset)-1 );
                           Refresh(win1);

                           if TextEditMode then     --  if the character position is greater than length then pad it
                              while Integer(Current_Char) > SU.Length(Edline)+1 loop
                                 SU.Insert(Edline,SU.Length(Edline)+1," ");
                              end loop;
                           end if;


                           Ada.Strings.Unbounded.Insert (Source => Edline,
                                                         Before => Integer(Current_Char),
                                                         New_Item => ("" & Ch));

                           Current_Char := Current_Char +1;

                           if Integer(Current_Char)-ScreenOffset = Integer(Columns-StartColumn)+1 then
                              ScreenOffset := ScreenOffset + (Integer(Columns-StartColumn)-1);
                           end if;
                        end if;
                     end if;
                  end if;

            end case;
         end if;
      end loop;

   end Line_Editor;





   procedure Text_Editor (win1 : Window;
                          TopLine : Line_Position;
                          BottomLine :Line_Position;
                          MaxLines : Integer) is

      curs,TempCurs : Cursor;
      CurrentLine : Line_Position := 0;
      TermLnth : Line_Position;
      TermWdth : Column_Position;
      EditBuffer,CarryOver,Remainder : Unbounded_String;
      endpoint : Integer;
      Cancelled : Boolean := False;



      procedure Scroll_Up is
      begin
         Move_Cursor(win1,Line   => TopLine,Column => 0);
         Delete_Line(win1);
         Move_Cursor(win1,Line   => BottomLine,Column => 0);
         Insert_Line(win1);
         Refresh(win1);
      end Scroll_Up;

      procedure Scroll_Down is
      begin
         Move_Cursor(win1,Line   => BottomLine,Column => 0);
         Delete_Line(win1);
         Move_Cursor(win1,Line   => TopLine,Column => 0);
         Insert_Line(win1);
         Refresh(win1);
      end Scroll_Down;

      procedure Redraw_Screen is
         curs2 : Cursor;
         LineNum : Line_Position := 0;

      begin
        -- curs := Text_Buffer.First;
         curs2 := curs;
         for i in 1 .. CurrentLine loop
            if curs2 /= Text_Buffer.First then
               String_List.Previous(curs2);
            end if;
         end loop;

         loop

            if Length(Element(curs2)) > Integer(TermWdth)-1 then
               endpoint := Integer(TermWdth)-1;
            else
               endpoint := Length(Element(curs2));
            end if;

            if endpoint > 0 then
               Add(win1,
                   Column => 0,Line => LineNum + TopLine,
                   Str => Slice(Element(curs2),1,endpoint) );
            else
               Move_Cursor(win1,Line => LineNum + TopLine,Column => 0);
            end if;
            Clear_To_End_Of_Line(win1);

            LineNum := LineNum +1;
            if LineNum+ TopLine > BottomLine then
               exit;
            elsif curs2 = Text_Buffer.Last then
               exit;
            else
               String_List.Next(curs2);
            end if;

         end loop;
         Refresh;
      end Redraw_Screen;



   begin

      Get_Size(Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
      -- Text_Buffer.Clear;
      Text_Buffer.Append(To_Unbounded_String(""));
      curs := Text_Buffer.First;
      Current_Char := 1;


      loop
         Get_Size(Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
        Redraw_Screen;

         EditBuffer := Element(curs);
         Line_Editor(win1,StartLine => TopLine + CurrentLine,
                     StartColumn => 0,
                     EditLength => TermWdth-1,MaxLength => 1000, -- Integer(Wdth-1),
                     Edline => EditBuffer,TextEditMode => True);
         Text_Buffer.Replace_Element(curs,New_Item => EditBuffer);


         if c in Special_Key_Code'Range then
            case c is
            when Key_Cursor_Down =>

               if curs /= Text_Buffer.Last then
                  String_List.Next(curs);
                  if CurrentLine < BottomLine-TopLine then
                     CurrentLine := CurrentLine + 1;
                  else
                     Scroll_Up;
                  end if;
               end if;

               -- Avoids cursor being beyond edge of screen crash on Down Arrow for long lines
               if Texaco.Current_Char > TermWdth-1 then
                  Texaco.Current_Char := TermWdth-1;
               end if;

            when Key_Cursor_Up =>
               if curs /= Text_Buffer.First then
                  String_List.Previous(curs);
                  if CurrentLine > 0 then
                     CurrentLine := CurrentLine - 1;
                  else
                     Scroll_Down;
                  end if;
               end if;

               -- Avoids cursor being beyond edge of screen crash on Up Arrow for long lines
               if Texaco.Current_Char > TermWdth-1 then
                  Texaco.Current_Char := TermWdth-1;
               end if;
            when Key_Shift_Delete_Char =>

               TempCurs := curs;
               if TempCurs /= Text_Buffer.Last then
                  String_List.Next(curs);
                  Text_Buffer.Delete(TempCurs);
                  Delete_Line(win1);
                  Move_Cursor(win1,Line   => BottomLine,Column => 0);
                  Insert_Line(win1);
               elsif TempCurs /= Text_Buffer.First then
                  String_List.Previous(curs);
                  if CurrentLine > 0 then
                     CurrentLine := CurrentLine - 1;
                  else
                     Scroll_Down;
                  end if;
                  Text_Buffer.Delete(TempCurs);
                  Delete_Line(win1);
                  Move_Cursor(win1,Line   => BottomLine,Column => 0);
                  Insert_Line(win1);
               end if;



               Refresh(win1);

            when others => null;
            end case;
         elsif c in Real_Key_Code'Range then

            case Character'Val (c) is
            when CR | LF =>

               -- Avoids cursor being beyond the end of the line crash on Caridge return for long lines
               if Integer(Current_Char) > SU.Length(Element(curs)) then
                  Current_Char := Column_Position(SU.Length(Element(curs))+1);
               end if;

               CarryOver :=To_Unbounded_String( SU.Slice(Source => Element(curs),
                                                         Low => Integer(Current_Char),
                                                         High => SU.Length(Element(curs))));

               Remainder :=To_Unbounded_String( SU.Slice(Source => Element(curs),
                                                         Low => 1,
                                                         High => Integer(Current_Char)-1 ));

               if curs = Text_Buffer.Last then
                  Text_Buffer.Replace_Element(curs,New_Item => Remainder);
                  Text_Buffer.Append(CarryOver);
                  curs := Text_Buffer.Last;
               else
                  Text_Buffer.Replace_Element(curs,New_Item => Remainder);
                  String_List.Next(curs);
                  Text_Buffer.Insert(Before => curs,New_Item => CarryOver);
                  String_List.Previous(curs);
               end if;
               Current_Char := 1;

               if CurrentLine < BottomLine-TopLine then
                  CurrentLine := CurrentLine + 1;
               else
                  Scroll_Up;
               end if;


            when ESC =>
               Save := Display_Warning.GetYN("Save & exit(Y)  Discard & exit(N)");
               if not Display_Warning.Cancel then
                  exit;
               end if;

            when others => null;
            end case;

         end if;


      end loop;

   end Text_Editor;

   procedure Dump_List is
      LineNum : Line_Position := 0;
      TermLnth : Line_Position;
      TermWdth : Column_Position;
      procedure Print(Position : Cursor) is
      begin
         Add(Standard_Window,
             Column => 0,Line => LineNum,
             Str => To_String(Element(Position)));
         Refresh;
         if LineNum < TermLnth-1 then
            LineNum := LineNum + 1;
         else
            Move_Cursor(Line => 0,Column => 0);
            Delete_Line;
            Refresh;
         end if;
      end Print;
   begin
      Get_Size(Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
      Text_Buffer.Iterate(Print'access);
   end Dump_List;


end Texaco;
