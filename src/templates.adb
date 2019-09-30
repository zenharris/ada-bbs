with Ada.Text_IO;            use Ada.Text_IO;
with Display_Warning;
with Dbase.Scroller;
-- with Dbase.DrackSpace;

package body Templates is


  -- package Drack is new Dbase.DrackSpace;



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


   function Xlat_Line (LineNum : Integer;InLine : Unbounded_String;ReadEditFields : Boolean := False) return Unbounded_String is
      scratch : Unbounded_String := InLine;
      RetString : Unbounded_String := InLine;
      FldName : Unbounded_String;

      Pattern : String := "@(\d+)<+";

      Regex : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile(Pattern); -- (1024);
      FindNoedit :GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("\:noedit\:");

      Matches, Matches2 : GNAT.Regpat.Match_Array (0 .. 1);
      FieldSize, FieldIndex : Integer;
      tmpbool : Boolean;

   begin

      GNAT.Regpat.Match (Regex, To_String(scratch) , Matches);

      while Matches (0) /= GNAT.Regpat.No_Match loop

         FieldSize := Matches(0).Last - Matches(0).First;
         FieldIndex := Integer'Value(SU.Slice(scratch,Matches(1).First,Matches(1).Last));

         retString := To_Unbounded_String(SU.Slice(scratch,1,Matches(0).First-1));

         FldName := To_Unbounded_String(SU.Slice(FieldsList.Element(FieldIndex),1,SU.Index(FieldsList.Element(FieldIndex),":")-1));

         if ReadEditFields then
            GNAT.Regpat.Match (FindNoedit, To_String(FieldsList.Element(FieldIndex)), Matches2);

            tmpbool := Matches2(0) /= GNAT.Regpat.No_Match;

            EditFieldsList.Append ((FldName,Line_Position(LineNum),
                                   Column_Position(Matches(0).First),
                                   FieldSize,False,tmpbool));
         end if;

         -- retString := retString & CharPad(To_Unbounded_String(Fld(CI,FldName)),FieldSize) ;
         retString := retString & CharPad(Current_Record(FldName),FieldSize) ;
         retString := retString & SU.Slice(scratch,Matches(0).Last+1,SU.Length(scratch));

         scratch := RetString;
         GNAT.Regpat.Match (Regex, To_String(scratch) , Matches);
      end loop;

      return retString;

   end Xlat_Line;

   procedure Read_Config_Dictionary(TableName : string) is

      File : File_Type;
      FileName : string := TableName & ".dict";

      scratch : Unbounded_String;
   begin

      Open (File => File,
            Mode => In_File,
            Name => FileName);

      Clear(ScreenList);
      Clear(FieldsList);

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
   end Read_Config_Dictionary;



   procedure Read_Current_Record (CI : Direct_Cursor; FieldsList : Screen_Vector.Vector) is
      FieldName : Unbounded_String;
   begin
      Clear(Current_Record);
      for i in 0..FieldsList.Length-1 loop
         FieldName := To_Unbounded_String(SU.Slice(FieldsList.Element(Integer(i)),1,SU.Index(FieldsList.Element(Integer(i)),":")-1));
         Current_Record.Include(FieldName,To_Unbounded_String(Fld(CI,FieldName)));
      end loop;
   end Read_Current_Record;

   procedure Init_Current_Record (FieldsList : Screen_Vector.Vector) is
      FieldName : Unbounded_String;
   begin
      Clear(Current_Record);
      for i in 0..FieldsList.Length-1 loop
         FieldName := To_Unbounded_String(SU.Slice(FieldsList.Element(Integer(i)),1,SU.Index(FieldsList.Element(Integer(i)),":")-1));
         Current_Record.Include(FieldName,To_Unbounded_String(""));
      end loop;
   end Init_Current_Record;

   procedure Set_Default (Fldnme : String; Default : String) is
      UpdateRecord : Edit_Fields_Record;
   begin

      Current_Record(To_Unbounded_String(Fldnme)) := To_Unbounded_String(Default);
      for i in 0..Integer(EditFieldsList.Length) loop
         if Fldnme = EditFieldsList.Element(i).Name then
            UpdateRecord := EditFieldsList.Element(i);
            UpdateRecord.Edited := True;
            EditFieldsList.Replace_Element(i,UpdateRecord);
            return;
         end if;
      end loop;
      Display_Warning.Warning("No Field "& Fldnme);

   end Set_Default;


   procedure Read_Edit_Fields is
      scratch : Unbounded_String;
   begin
    Clear(EditFieldsList);

      for i in 0..ScreenList.Length-1 loop
              scratch := Xlat_Line(Integer(i)+1,ScreenList.Element(Integer(i)),True);
      end loop;

   end Read_Edit_Fields;

   EmptyScroll : Boolean;

   function Initialise (CI :Direct_Cursor; TableName : String;NewRecord : Boolean := False) return Boolean is

      Width : Column_Position := 90;
      Length : Line_Position := 25;

      TermLnth : Line_Position;
      TermWdth : Column_Position;
   begin
     SaveTableName := To_Unbounded_String(TableName);

      Read_Config_Dictionary(TableName);
      EmptyScroll := False;
      if NewRecord then
         Init_Current_Record(FieldsList);
         EmptyScroll := True;
      elsif CI.Has_Row then
         Read_Current_Record(CI,FieldsList);
      else
         Init_Current_Record(FieldsList);
         EmptyScroll := True;
      end if;

      Read_Edit_Fields;

      Get_Size(Standard_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);

      -- Width := Column_Position(SU.Length(Element(Scrl_Buffer.First).Prompt) + 5);
      Length := Line_Position(ScreenList.Length+2);
      if Width < TermWdth then

         Display_Window := Sub_Window(Win => Standard_Window,
                                      Number_Of_Lines => Length,
                                      Number_Of_Columns => Width,
                                      First_Line_Position => (TermLnth - Length) / 2,
                                      First_Column_Position => (TermWdth - Width) / 2);

         Clear(Display_Window);
         Box(Display_Window);
         Refresh(Display_Window);
      else
         Display_Warning.Warning("Terminal not wide enough");
         return False;
      end if;

      return True;
   end Initialise;

   RecordEdited : Boolean;

   procedure Edit_Form is
      i : Integer := 0;

      Edline : Unbounded_String;
      UpdateRecord : Edit_Fields_Record;
      NoEditFields : Boolean := False;
      c : Key_Code;
   begin
      RecordEdited := False;

      while EditFieldsList.Element(i).NoEdit loop
         if i < Integer(EditFieldsList.Length)-1 then
            i := i + 1;
         else
            NoEditFields := True;
            exit;
         end if;
      end loop;

      if not NoEditFields then

         loop
            if not EditFieldsList.Element(i).NoEdit then
               Edline := Current_Record(EditFieldsList.Element(i).Name);
               Texaco.Line_Editor(Display_Window,
                                  StartLine => EditFieldsList.Element(i).Row,
                                  StartColumn =>  EditFieldsList.Element(i).Col,
                                  Editlength => Column_Position(EditFieldsList.Element(i).Length+1),
                                  Edline => Edline,
                                  MaxLength => EditFieldsList.Element(i).Length,
                                  SuppressSpaces => False);
               if Edline /= Current_Record(EditFieldsList.Element(i).Name) then
                  RecordEdited := true;
                  UpdateRecord := EditFieldsList.Element(i);
                  UpdateRecord.Edited := True;
                  EditFieldsList.Replace_Element(i,UpdateRecord);
                  Current_Record(EditFieldsList.Element(i).Name) := Edline;
               end if;
            end if;



            if Texaco.c in Special_Key_Code'Range then
               case Texaco.c is
               when Key_Cursor_Down | Key_Cursor_Right =>
                  if i < Integer(EditFieldsList.Length)-1 then
                     i := i + 1;
                  else
                     i := 1;
                  end if;
               when Key_Cursor_Up | Key_Cursor_Left =>
                  if i > 1 then
                     i := i - 1;
                  else
                     i := Integer(EditFieldsList.Length)-1;
                  end if;
               when others => null;
               end case;
            elsif Texaco.c in Real_Key_Code'Range then
               case Character'Val (Texaco.c) is
               when CR | LF | HT =>
                  if i < Integer(EditFieldsList.Length)-1 then
                     i := i + 1;
                  else
                     i := 1;
                  end if;
               when ESC => exit;
                  when others => null;
               end case;
            end if;

         end loop;
      else
         loop
            c := Texaco.GetKey;
            if c in Special_Key_Code'Range then
               null;
            elsif c in Real_Key_Code'Range then
               exit when Character'Val (c) = ESC;
            end if;
         end loop;

      end if;

   end Edit_Form;

   function MkInsert return String is

      Fieldnames,Fieldvalues : Unbounded_String;
      SQLstatement : Unbounded_String;
      Fields : Unbounded_String;
      Stmt : Prepared_Statement;
      Frst : Boolean := True;
   begin

      for i in 1..Integer(EditFieldsList.Length-1) loop
         if EditFieldsList.Element(i).Edited then
            if not Frst then
               Fieldnames := Fieldnames & ",";
               Fieldvalues := Fieldvalues & ",";
            else
               Frst := False;
            end if;

            Fieldnames := Fieldnames & EditFieldsList.Element(i).Name;
            Fieldvalues := Fieldvalues & "'" &Current_Record(EditFieldsList.Element(i).Name)&"'";
         --   if i < Integer(EditFieldsList.Length-1) then
         --      Fieldnames := Fieldnames & ",";
         --      Fieldvalues := Fieldvalues & ",";
         --   end if;
         end if;

      end loop;

      SQLstatement := SQLstatement & "INSERT INTO " & SaveTableName & " (";
      SQLstatement := SQLstatement & Fieldnames & ") VALUES (" & Fieldvalues & ")";

      Add (Standard_Window,
              Line => 1,
              Column => 1,
           Str => To_String(SQLstatement));
      refresh;


      return To_String(SQLstatement);

   end MkInsert;

   function MkUpdate return String is
      Fieldnames,Fieldvalues : Unbounded_String;
      SQLstatement : Unbounded_String;
      Fields : Unbounded_String;
      Frst : Boolean := True;
   begin

      for i in 1..Integer(EditFieldsList.Length-1) loop
         if EditFieldsList.Element(i).Edited then
            if not Frst then
               Fields := Fields & ",";
            else
               Frst := False;
            end if;
            Fields := Fields & EditFieldsList.Element(i).Name & " = ";
            Fields := Fields & "'"& Current_Record(EditFieldsList.Element(i).Name)& "'";
            --    if i < Integer(EditFieldsList.Length-1) then
            --       Fields := Fields & ",";
            --    end if;
         end if;

      end loop;

      SQLstatement := SQLstatement & "UPDATE " & SaveTableName & " SET " & Fields & " WHERE ";
      SQLstatement := SQLstatement & EditFieldsList.Element(0).Name & " = '";
      SQLstatement := SQLstatement & Current_Record(EditFieldsList.Element(0).Name) & "'";

       Add (Standard_Window,
              Line => 1,
              Column => 1,
           Str => To_String(SQLstatement));
      refresh;


      return To_String(SQLstatement);

   end MkUpdate;


   procedure Edit_Page is  --(CI :Direct_Cursor; TableName : String) is
      Stmt : Prepared_Statement;
   begin

      -- Clear(EditFieldsList);

      for i in 0..ScreenList.Length-1 loop
         Add (Display_Window,
              Line => Line_Position(1+i),
              Column => 1,
              Str => To_String(Xlat_Line(Integer(i)+1,ScreenList.Element(Integer(i)),False )));
      end loop;

      Refresh(Display_Window);

      Edit_Form;

      if EmptyScroll then
         Stmt:= Prepare (MkInsert);
      else
         Stmt:= Prepare (MkUpdate);
      end if;


      Current_Record_Updated := False;
      if RecordEdited and then Display_Warning.GetYN("Save Changes Y/N") then
         Dbase.DB.Execute(Stmt);
         Dbase.DB.Commit;

         if Dbase.DB.Success then
            Current_Record_Updated := True;
            Display_Warning.Warning("Update Successful");
         else
            Display_Warning.Warning("Update Failed");
         end if;
      end if;

      Close_Page;

      --  end if;

   end Edit_Page;




   procedure Redraw_Page is
   begin


      for i in 0..ScreenList.Length-1 loop
         Add (Display_Window,
              Line => Line_Position(1+i),
              Column => 1,
              Str => To_String(Xlat_Line(Integer(i)+1,ScreenList.Element(Integer(i)),False )));
      end loop;
      Box(Display_Window);
      Refresh(Display_Window);


   end Redraw_Page;

   procedure Close_Page is
   begin

      Clear(Display_Window);
      Refresh(Display_Window);

      Delete (Win => Display_Window);


   end Close_Page;

   procedure Recycle is
      Stmt : Prepared_Statement;
      CIB : Direct_Cursor;
      SQLstatement : Unbounded_String;
   begin

      SQLstatement := SQLstatement & "SELECT * FROM " & SaveTableName & " WHERE ";
      SQLstatement := SQLstatement & EditFieldsList.Element(0).Name & " = '";
      SQLstatement := SQLstatement & Current_Record(EditFieldsList.Element(0).Name) & "'";

    --   Add (Standard_Window,
    --          Line => 1,
    --          Column => 1,
    --       Str => To_String(SQLstatement));
    --  refresh;

      Stmt:= Prepare (To_String(SQLstatement), Index_By => Field_Index'First);

      CIB.Fetch (Dbase.DB, Stmt);

      if CIB.Has_Row then
         Read_Current_Record (CIB,FieldsList);
      else
         Display_Warning.Warning("ReRead Failed");
      end if;



   end Recycle;


   procedure Set_Dest_Coords is

      Width : Column_Position := 50;
      Length : Line_Position := 10;

      TermLnth : Line_Position;
      TermWdth : Column_Position;
      Display_Window : Window;
      destx, desty, destz : Unbounded_String;
      SQLstatement : Unbounded_String;
      Stmt : Prepared_Statement;
   begin
    Get_Size(Standard_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);

      -- Width := Column_Position(SU.Length(Element(Scrl_Buffer.First).Prompt) + 5);
      -- Length := Line_Position(ScreenList.Length+2);
      if Width < TermWdth then

         Display_Window := Sub_Window(Win => Standard_Window,
                                      Number_Of_Lines => Length,
                                      Number_Of_Columns => Width,
                                      First_Line_Position => (TermLnth - Length) / 2,
                                      First_Column_Position => (TermWdth - Width) / 2);

         Clear(Display_Window);
         Box(Display_Window);
         Refresh(Display_Window);
      else
         Display_Warning.Warning("Terminal not wide enough");

      end if;


      Add (Display_Window,
              Line => 2,
              Column => 1,
           Str => "Dest X : ");
      refresh(Display_Window);
      destx := Current_Record(To_Unbounded_String("loc_x"));
      Texaco.Line_Editor(Display_Window,
                         StartLine => 2,
                         StartColumn =>  10,
                         Editlength => 32,
                         Edline => destx,
                         MaxLength => 31,
                         SuppressSpaces => True);
       Add (Display_Window,
              Line => 3,
              Column => 1,
           Str => "Dest Y : ");
      refresh(Display_Window);
      desty := Current_Record(To_Unbounded_String("loc_y"));
      Texaco.Line_Editor(Display_Window,
                         StartLine => 3,
                         StartColumn =>  10,
                         Editlength => 32,
                         Edline => desty,
                         MaxLength => 31,
                         SuppressSpaces => True);
       Add (Display_Window,
              Line => 4,
              Column => 1,
           Str => "Dest Z : ");
      refresh(Display_Window);
      destz := Current_Record(To_Unbounded_String("loc_z"));
      Texaco.Line_Editor(Display_Window,
                         StartLine => 4,
                         StartColumn =>  10,
                         Editlength => 32,
                         Edline => destz,
                         MaxLength => 31,
                         SuppressSpaces => True);

      SQLstatement := SQLstatement & "UPDATE " & SaveTableName & " SET ";
      SQLstatement := SQLstatement & "dest_x = '"& destx &"' , dest_y = '" & desty &"' , dest_z = '"&destz&"'";
      SQLstatement := SQLstatement & " WHERE " & EditFieldsList.Element(0).Name & " = '";
      SQLstatement := SQLstatement & Current_Record(EditFieldsList.Element(0).Name) & "'";


       Add (Standard_Window,
              Line => 1,
              Column => 1,
           Str => To_String(SQLstatement));
      refresh;

      Stmt:= Prepare (To_String(SQLstatement));

      Dbase.DB.Execute(Stmt);
      Dbase.DB.Commit;

      if Dbase.DB.Success then
         Display_Warning.Warning("Dest Coords Set");
      else
         Display_Warning.Warning("Update Failed");
      end if;


      Clear(Display_Window);
      Refresh(Display_Window);

      Delete (Win => Display_Window);



   end Set_Dest_Coords;


   procedure Jump_Dest_Coords is

      Width : Column_Position := 50;
      Length : Line_Position := 10;

      TermLnth : Line_Position;
      TermWdth : Column_Position;
      Display_Window : Window;
      destx, desty, destz : Unbounded_String;
      savex, savey, savez : Unbounded_String;
      SQLstatement : Unbounded_String;
      Stmt : Prepared_Statement;
      Next : Time;
      D    : Duration := 3.0;
      Now : Time := Clock;

   begin
    Get_Size(Standard_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);

      -- Width := Column_Position(SU.Length(Element(Scrl_Buffer.First).Prompt) + 5);
      -- Length := Line_Position(ScreenList.Length+2);
      if Width < TermWdth then

         Display_Window := Sub_Window(Win => Standard_Window,
                                      Number_Of_Lines => Length,
                                      Number_Of_Columns => Width,
                                      First_Line_Position => (TermLnth - Length) / 2,
                                      First_Column_Position => (TermWdth - Width) / 2);

         Clear(Display_Window);
         Box(Display_Window);
         Refresh(Display_Window);
      else
         Display_Warning.Warning("Terminal not wide enough");
      end if;

      Set_Character_Attributes(Display_Window, (Reverse_Video => True,Blink => True,others => False));
      Add (Win => Display_Window,
           Line => 2,
           Column => 2,
           Str => "Spooling Up Jump Engine");
      Refresh(Display_Window);
      Set_Character_Attributes(Display_Window, Normal_Video);

      Now := Clock;
      Next := Now + D;
      delay until Next;

      Add (Win => Display_Window,
           Line => 2,
           Column => 2,
           Str => "Jump Engine Ready To Jump");
      Refresh(Display_Window);


      Set_Character_Attributes(Display_Window, (Reverse_Video => True,Blink => True,others => False));
      Add (Win => Display_Window,
           Line => 3,
           Column => 2,
           Str => "Jump Underway");
      Refresh(Display_Window);
      Set_Character_Attributes(Display_Window, Normal_Video);

      Now := Clock;
      Next := Now + D;
      delay until Next;
      Add (Win => Display_Window,
           Line => 3,
           Column => 2,
           Str => "Jump Completed");
      Refresh(Display_Window);

      Now := Clock;
      Next := Now + D;
      delay until Next;

    --  EXEC SQL
    --  SELECT loc_x, loc_y, loc_z INTO :savex, :savey, savez
    --       FROM ships WHERE ship_id = 3;

      savex := Current_Record(To_Unbounded_String("dest_x"));
      savey := Current_Record(To_Unbounded_String("dest_y"));
      savez := Current_Record(To_Unbounded_String("dest_z"));
      destx := Current_Record(To_Unbounded_String("dest_x"));
      desty := Current_Record(To_Unbounded_String("dest_y"));
      destz := Current_Record(To_Unbounded_String("dest_z"));


      SQLstatement := SQLstatement & "UPDATE " & SaveTableName & " SET ";
      SQLstatement := SQLstatement & "loc_x = '"& destx &"',loc_y = '" & desty &"',loc_z = '"&destz&"'";
      SQLstatement := SQLstatement & ",dest_x = '"& savex &"',dest_y = '" & savey &"',dest_z = '"&savez&"'";
      SQLstatement := SQLstatement & " WHERE " & EditFieldsList.Element(0).Name & " = '";
      SQLstatement := SQLstatement & Current_Record(EditFieldsList.Element(0).Name) & "'";


       Add (Standard_Window,
              Line => 1,
              Column => 1,
           Str => To_String(SQLstatement));
      refresh;

      Stmt:= Prepare (To_String(SQLstatement));

      Dbase.DB.Execute(Stmt);
      Dbase.DB.Commit;

      if not Dbase.DB.Success then
         Display_Warning.Warning("Panic Jump Failed");
      end if;


      Clear(Display_Window);
      Refresh(Display_Window);

      Delete (Win => Display_Window);



   end Jump_Dest_Coords;

   L_Ack_Tail : string := ":null:%10s| %-30s| %-30s|:ship_id:ship_name:captain:";

   procedure Radar_Scan is
      RadRange : Long_Long_Integer := Long_Long_Integer(1000000000000000000);
      XLocus,YLocus,ZLocus : Unbounded_String;
      L_AckStatement : Unbounded_String;
   begin

      XLocus := Current_Record(To_Unbounded_String("loc_x"));
      YLocus := Current_Record(To_Unbounded_String("loc_y"));
      ZLocus := Current_Record(To_Unbounded_String("loc_z"));



     L_AckStatement := L_AckStatement & "SELECT * FROM " & SaveTableName & " WHERE "
      & "loc_x between "& XLocus &" - 10000 AND "& XLocus &" + 10000"
        & " AND loc_y between "& YLocus &" - 10000 AND " & YLocus &" + 10000"
        & " AND loc_z between "& ZLocus &" - 10000 AND " & ZLocus &" + 10000 order by loc_x,loc_y,loc_z"
        & L_Ack_Tail;

      Add (Standard_Window,
              Line => 1,
              Column => 1,
           Str => To_String(L_AckStatement));
      refresh;

      Dbase.Scroller.Definition_Ptr := 1;
      Dbase.Scroller.Scroll(To_String(L_AckStatement));


   end Radar_Scan;



   procedure dummy is
   begin
      null;
   end dummy;


   Navigation : Process_Menu.Menu_Type  :=
     ((new String'("Set Destination"),Set_Dest_Coords'Unrestricted_Access),
      (new String'("Start Engines"),dummy'Unrestricted_Access),
      (new String'("Initiate Jump"),Jump_Dest_Coords'Unrestricted_Access),
      (new String'("Radar Scan"),Radar_Scan'Unrestricted_Access));

   Radar : Process_Menu.Menu_Type  :=
     ((new String'("Radar Screen"),Radar_Scan'Unrestricted_Access),
      (new String'("Null"),dummy'Unrestricted_Access));


   procedure Command_Screen is
      c : Key_Code;
      StopOverwrite : Boolean := False;

      task Display_Current_Time is
         entry Start;
      end Display_Current_Time;

      task body Display_Current_Time is
         Next : Time;
         D    : Duration := 1.0;
         Now : Time := Clock;

      begin
         accept Start;

         loop
            Now := Clock;

            Next := Now + D;

            Add (Win => Display_Window,Line => 1,Column => 48,Str => "Cronometer " & Image (Now));

            if not StopOverwrite then
               Recycle;
               Redraw_Page;
           -- else
           --    Refresh(Display_Window);
            end if;

            delay until Next;
         end loop;
      end Display_Current_Time;


   begin

      Add (Line => Lines - 2,Column => 1, Str => "1 Navig  |2 Engine |3 Radar  |4 Weapons|");
      Refresh;
      Display_Current_Time.Start;
      loop
         Redraw_Page;
         StopOverwrite := False;
         c := Texaco.GetKey;  -- Get_Keystroke;
         StopOverwrite := True;
         if c in Special_Key_Code'Range then
            case c is
            when Key_F1 =>

               Process_Menu.Open_Menu (Function_Number => 1,Menu_Array => Navigation );

            when Key_F2 =>
               Process_Menu.Open_Menu (Function_Number => 2,Menu_Array => Navigation);
            when Key_F3 =>
               Process_Menu.Open_Menu (Function_Number => 3,Menu_Array => Radar);
            when Key_F4 =>
               Process_Menu.Open_Menu (Function_Number => 4,Menu_Array => Navigation);
               when Key_F5 =>
                  Recycle;

            when others => null;
            end case;
         elsif c in Real_Key_Code'Range then
            case Character'Val (c) is
               when LF | CR =>  null;
               when ESC => Exit;
                  when others => null;
            end case;
         end if;
      end loop;
      Close_Page;
      Abort Display_Current_Time;
   end Command_Screen;




end Templates;
