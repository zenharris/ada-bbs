with Ada.Text_IO;            use Ada.Text_IO;
with Display_Warning;

package body Templates is
   InLine : Unbounded_String :=
     To_Unbounded_String("Customer ID : @0<<<<<  Name : @1<<<<<<<<<<<<<<<<<<<<<< @2<<<<<<<<<<<<<<<<<<<<<<<<<<");

   procedure Display_Page (CI :Direct_Cursor; TableName : String) is
   --   c : Key_Code;
      FieldSize, FieldIndex : Integer;
      File : File_Type;
      FileName : string := TableName & ".dict";
      ScreenList : Screen_Vector.Vector;
      FieldsList : Screen_Vector.Vector;
      EditFieldsList : Edit_Fields_Vector.Vector;
      TermLnth : Line_Position;
      TermWdth : Column_Position;
      Display_Window : Window;
      Edline : Unbounded_String;

      Pattern : String := "@(\d+)<+";

      Regex : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile(Pattern); -- (1024);
      Matches : GNAT.Regpat.Match_Array (0 .. 1);

      function Fld (CI : Direct_Cursor; FldNme : Unbounded_String) return String is
      begin
         for i in 0..CI.Field_Count-1 loop
            if CI.Field_Name(i) = To_String(FldNme) then
               return CI.Value(i);
            end if;
         end loop;
         Display_Warning.Warning("No Field " & To_String(FldNme));
         return To_String(FldNme) & " Not Found";
      end;

      function CharPad(InStr : Unbounded_String; PadWidth : Integer) return Unbounded_String is
         padstr,retString : Unbounded_String;
      begin
         if SU.Length(InStr) <= PadWidth then
            for i in SU.Length(InStr) .. PadWidth loop
               padstr := padstr & " ";
            end loop;
            retString := Instr & padstr;
         else
            retString := To_Unbounded_String(SU.Slice(InStr,1,Padwidth));
         end if;

         return retString;

      end CharPad;

      function Xlat_Line (LineNum : Integer;InLine : Unbounded_String;ReadEditFields : Boolean := False) return Unbounded_String is
         scratch : Unbounded_String := InLine;
         RetString : Unbounded_String := InLine;
         FldName : Unbounded_String;
      begin

         GNAT.Regpat.Match (Regex, To_String(scratch) , Matches);

         while Matches (0) /= GNAT.Regpat.No_Match loop

            FieldSize := Matches(0).Last - Matches(0).First;
            FieldIndex := Integer'Value(SU.Slice(scratch,Matches(1).First,Matches(1).Last));

            retString := To_Unbounded_String(SU.Slice(scratch,1,Matches(0).First-1));

            FldName := To_Unbounded_String(SU.Slice(FieldsList.Element(FieldIndex),1,SU.Index(FieldsList.Element(FieldIndex),":")-1));

            if ReadEditFields then
               -- Clear(EditFieldsList);
               EditFieldsList.Append ((FldName,Line_Position(LineNum),Column_Position(Matches(0).First),FieldSize));
            end if;

            retString := retString & CharPad(To_Unbounded_String(Fld(CI,FldName)),FieldSize) ;
            retString := retString & SU.Slice(scratch,Matches(0).Last+1,SU.Length(scratch));

            scratch := RetString;
            GNAT.Regpat.Match (Regex, To_String(scratch) , Matches);
         end loop;

         return retString;

      end Xlat_Line;

      procedure Read_Current_Record (CI : Direct_Cursor; FieldsList : Screen_Vector.Vector) is
          FieldName : Unbounded_String;
      begin
         Clear(Current_Record);
         for i in 0..FieldsList.Length-1 loop
            FieldName := To_Unbounded_String(SU.Slice(FieldsList.Element(Integer(i)),1,SU.Index(FieldsList.Element(Integer(i)),":")-1));
            Current_Record.Include(FieldName,To_Unbounded_String(Fld(CI,FieldName)));
         end loop;
      end Read_Current_Record;


      Width : Column_Position := 90;
      Length : Line_Position := 25;

   begin

      -- GNAT.Regpat.Compile (Regex, Pattern);

      declare
         scratch : Unbounded_String;
      begin

         Open (File => File,
               Mode => In_File,
               Name => FileName);

         While not  End_Of_File (File) loop

            scratch := SUIO.Get_Line (File);
            exit when SU.Index(scratch,"%FIELD_DEF%") = 1;
            ScreenList.Append (scratch);

         end loop;
         while not End_Of_File(File) loop

            scratch := SUIO.Get_Line (File);
            exit when SU.Index(scratch,"%LIST_DEF%") = 1;
            FieldsList.Append (scratch);
         end loop;

         Close (File);
      end;


      Get_Size(Standard_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);

      -- Width := Column_Position(SU.Length(Element(Scrl_Buffer.First).Prompt) + 5);
      Length := Line_Position(ScreenList.Length+2);
      if Width < TermWdth then

         Display_Window := Sub_Window(Win => Standard_Window,
                                      Number_Of_Lines => Length,
                                      Number_Of_Columns => Width,
                                      First_Line_Position => 2, --(TermLnth - Length) / 2,
                                      First_Column_Position => 2); --(TermWdth - Width) / 2);

         Clear(Display_Window);
         Box(Display_Window);
         Refresh(Display_Window);

         Clear(EditFieldsList);

         for i in 0..ScreenList.Length-1 loop
            Add (Display_Window,
                 Line => Line_Position(1+i),
                 Column => 1,
                 Str => To_String(Xlat_Line(Integer(i)+1,ScreenList.Element(Integer(i)),True )));
         end loop;

         Refresh(Display_Window);


         Read_Current_Record(CI,FieldsList);


         -- for i in 0..EditFieldsList.Length-1 loop
         declare
            i : Integer := 0;
         begin
            loop
               Edline := Current_Record(EditFieldsList.Element(i).Name);
               Texaco.Line_Editor(Display_Window,
                                  StartLine => EditFieldsList.Element(i).Row,
                                  StartColumn =>  EditFieldsList.Element(i).Col,
                                  Editlength => Column_Position(EditFieldsList.Element(i).Length+1),
                                  Edline => Edline,
                                  MaxLength => EditFieldsList.Element(i).Length,
                                  SuppressSpaces => False);
               Current_Record(EditFieldsList.Element(i).Name) := Edline;

               if Texaco.c in Special_Key_Code'Range then
                  case Texaco.c is
                  when Key_Cursor_Down | Key_Cursor_Right =>
                     if i < Integer(EditFieldsList.Length)-1 then
                        i := i + 1;
                     end if;
                  when Key_Cursor_Up | Key_Cursor_Left =>
                     if i > 0 then
                        i := i - 1;
                     end if;
                  when others => null;
                  end case;
               elsif Texaco.c in Real_Key_Code'Range then
                  case Character'Val (Texaco.c) is
                     when CR | LF =>
                        if i < Integer(EditFieldsList.Length)-1 then
                           i := i + 1;
                        end if;
                  when ESC => exit;
                  when others => null;
                  end case;
               end if;

            end loop;
         end;


         -- c := Get_Keystroke;

         Clear(Display_Window);
         Refresh(Display_Window);

         Delete (Win => Display_Window);
      else
         Display_Warning.Warning("Terminal not wide enough");
      end if;

   end Display_Page;

end Templates;
