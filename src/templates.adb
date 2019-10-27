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

   function Fld (CI : Direct_Cursor; FldNme : String) return String is
   begin
      for i in 0..CI.Field_Count-1 loop
         if CI.Field_Name(i) = FldNme then
            return CI.Value(i);
         end if;
      end loop;
      Display_Warning.Warning("No Field " & FldNme);
      return FldNme & " Not Found";
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

      if Current_Record.Is_Empty then
         Clear(Current_Record);
         for i in 0..FieldsList.Length-1 loop
            FieldName := To_Unbounded_String(SU.Slice(FieldsList.Element(Integer(i)),1,SU.Index(FieldsList.Element(Integer(i)),":")-1));
            Current_Record.Include(FieldName,To_Unbounded_String(Fld(CI,FieldName)));
         end loop;
      else
         for i in 0..FieldsList.Length-1 loop
            FieldName := To_Unbounded_String(SU.Slice(FieldsList.Element(Integer(i)),1,SU.Index(FieldsList.Element(Integer(i)),":")-1));
            Current_Record(FieldName) := To_Unbounded_String(Fld(CI,FieldName));
         end loop;
      end if;


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

   function Initialise (CI :Direct_Cursor;
                        TableName : String;
                        NewRecord : Boolean := False;
                        NoWindow : Boolean := False) return Boolean is

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
         Dbase.MyLocX := Long_Long_Float'Value(Fld(CI,"loc_x"));
         Dbase.MyLocY := Long_Long_Float'Value(Fld(CI,"loc_y"));
         Dbase.MyLocZ := Long_Long_Float'Value(Fld(CI,"loc_z"));

      else
         Init_Current_Record(FieldsList);
         EmptyScroll := True;
      end if;

      Read_Edit_Fields;

      if not NoWindow then
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
      if DebugMode then
         Add (Standard_Window,
              Line => 2,
              Column => 1,
              Str => To_String(SQLstatement));
         refresh;
      end if;


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

      if DebugMode then
         Add (Standard_Window,
              Line => 2,
              Column => 1,
              Str => To_String(SQLstatement));
         refresh;
      end if;


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
            Display_Warning.Warning("Update Successful",D => 0.7);
         else
            Display_Warning.Warning("Update Failed",D => 0.7);
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

      Stmt:= Prepare (To_String(SQLstatement));

      CIB.Fetch (Dbase.DB_Background, Stmt);

      if CIB.Has_Row then
         Read_Current_Record (CIB,FieldsList);
      else
         Display_Warning.Warning("ReRead Failed",D => 0.7);
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
      targx,targy,targz,distance : Long_Long_Float;
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
      destx := Current_Record(To_Unbounded_String("dest_x"));
      loop
         Texaco.Line_Editor(Display_Window,
                            StartLine => 2,
                            StartColumn =>  10,
                            Editlength => 16,
                            Edline => destx,
                            MaxLength => 15,
                            SuppressSpaces => True,
                            Number => True);
         exit when destx /= "";
      end loop;


      Add (Display_Window,
           Line => 3,
           Column => 1,
           Str => "Dest Y : ");
      refresh(Display_Window);
      desty := Current_Record(To_Unbounded_String("dest_y"));
      loop
         Texaco.Line_Editor(Display_Window,
                            StartLine => 3,
                            StartColumn =>  10,
                            Editlength => 16,
                            Edline => desty,
                            MaxLength => 15,
                            SuppressSpaces => True,
                            Number => True);
         exit when desty /= "";
      end loop;
      Add (Display_Window,
           Line => 4,
              Column => 1,
           Str => "Dest Z : ");
      refresh(Display_Window);
      destz := Current_Record(To_Unbounded_String("dest_z"));
      loop
         Texaco.Line_Editor(Display_Window,
                            StartLine => 4,
                            StartColumn =>  10,
                            Editlength => 16,
                            Edline => destz,
                            MaxLength => 15,
                            SuppressSpaces => True,
                            Number => True);
         exit when destz /= "";
      end loop;

      targx := Long_Long_Float'Value(To_String(destx));
      targy := Long_Long_Float'Value(To_String(desty));
      targz := Long_Long_Float'Value(To_String(destz));

      distance := Value_Functions.Sqrt(((Dbase.MyLocX-targx)**2) +
                                       ((Dbase.MyLocY-targy)**2) +
                                       ((Dbase.MyLocZ-targz)**2));





      SQLstatement := SQLstatement & "UPDATE " & SaveTableName & " SET ";
      SQLstatement := SQLstatement & "dest_x = '"& destx &"' , dest_y = '" & desty &"' , dest_z = '"&destz&"'";
      SQLstatement := SQLstatement & " WHERE " & EditFieldsList.Element(0).Name & " = '";
      SQLstatement := SQLstatement & Current_Record(EditFieldsList.Element(0).Name) & "'";

      if DebugMode then
         Add (Standard_Window,
              Line => 2,
              Column => 1,
              Str => To_String(SQLstatement));
         refresh;
      end if;


      Stmt:= Prepare (To_String(SQLstatement));

      Dbase.DB.Execute(Stmt);
      Dbase.DB.Commit;

      if Dbase.DB.Success then
         Display_Warning.Warning("Dest Coords Set",D => 0.7);
      else
         Display_Warning.Warning("Update Failed",D => 0.7);
      end if;


      Clear(Display_Window);
      Refresh(Display_Window);

      Delete (Win => Display_Window);

   exception
      when CONSTRAINT_ERROR => null;
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

    --  Now := Clock;
    --  Next := Now + D;
    --  delay until Next;

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

      if DebugMode then
       Add (Standard_Window,
              Line => 2,
              Column => 1,
           Str => To_String(SQLstatement));
      refresh;
      end if;

      Stmt:= Prepare (To_String(SQLstatement));

      Dbase.DB.Execute(Stmt);
      Dbase.DB.Commit;

      if not Dbase.DB.Success then
         Display_Warning.Warning("Panic Jump Failed");
      else
         Dbase.MyLocX := Long_Long_Float'Value(To_String(destx));
         Dbase.MyLocY := Long_Long_Float'Value(To_String(desty));
         Dbase.MyLocZ := Long_Long_Float'Value(To_String(destz));
      end if;


      Clear(Display_Window);
      Refresh(Display_Window);

      Delete (Win => Display_Window);



   end Jump_Dest_Coords;



   L_Ack_Tail : string := ":null:%10s| %s => %s.%s.%s:ship_id:ship_name:loc_x:loc_y:loc_z:";

   procedure Radar_Scan is
      RadRange : Long_Long_Float := Long_Long_Float(999999999999999999);
      XLocus,YLocus,ZLocus : Unbounded_String;
      L_AckStatement : Unbounded_String;
   begin

      XLocus := Current_Record(To_Unbounded_String("loc_x"));
      YLocus := Current_Record(To_Unbounded_String("loc_y"));
      ZLocus := Current_Record(To_Unbounded_String("loc_z"));



     L_AckStatement := L_AckStatement & "SELECT * FROM " & SaveTableName & " WHERE "
      & "loc_x between "& XLocus  &" - 10000 AND "& XLocus &" + 10000"
        & " AND loc_y between "& YLocus &" - 10000 AND " & YLocus &" + 10000"
        & " AND loc_z between "& ZLocus &" - 10000 AND " & ZLocus &" + 10000 order by loc_x,loc_y,loc_z"
        & L_Ack_Tail;

     -- Add (Standard_Window,
     --        Line => 2,
     --        Column => 1,
     --     Str => To_String(L_AckStatement));
     -- refresh;

      Dbase.Scroller.Radar_Mode := True;
      Dbase.Scroller.Definition_Ptr := 1;
      Dbase.Scroller.Scroll(To_String(L_AckStatement),Down => 3,Left => 15,AltFunctions => True);
      Dbase.Scroller.Radar_Mode := False;

   end Radar_Scan;



   subtype Rand_Range is Integer range 1..6;   --Positive;
   package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);

   gen : Rand_Int.Generator;

   procedure Inflict_Damage (ShipID : Unbounded_String; DamageX : Integer := 1) is
      Stmt : Prepared_Statement;
      CIB : Direct_Cursor;
      SQLstatement, Damage_Report : Unbounded_String;
      navcom,jmpeng,engine,deflect,hull : Integer;
      locx,locy,locz,targx,targy,targz,distance : Long_Long_Float;
      scratch : Unbounded_String;

      D    : Duration := 0.4;
      Now : Time := Clock;
      Next : Time := Now + D;
      --   n : Integer;
   begin

     -- Recycle;

      SQLstatement := SQLstatement &
        "SELECT * FROM ships WHERE ship_id = " & ShipID &" FOR UPDATE";

    --   Add (Standard_Window,
    --          Line => 1,
    --          Column => 1,
    --       Str => To_String(SQLstatement));
    --  refresh;

      Stmt:= Prepare (To_String(SQLstatement));

      CIB.Fetch (Dbase.DB_Guns, Stmt);

      if CIB.Has_Row then
         navcom := Integer'Value(Fld(CIB,"navcom_funct"));
         jmpeng := Integer'Value(Fld(CIB,"jmpeng_funct"));
         engine := Integer'Value(Fld(CIB,"engine_funct"));
         deflect:= Integer'Value(Fld(CIB,"deflect_funct"));
         hull   := Integer'Value(Fld(CIB,"hull_value"));

         locx := Dbase.MyLocX;
         locy := Dbase.MyLocY;
         locz := Dbase.MyLocZ;

         targx := Long_Long_Float'Value(Fld(CIB,"loc_x"));
         targy := Long_Long_Float'Value(Fld(CIB,"loc_y"));
         targz := Long_Long_Float'Value(Fld(CIB,"loc_z"));

         distance := Value_Functions.Sqrt(((locx - targx)**2) + ((locy-targy)**2) + ((locz-targz)**2));

         if (targx+targy+targz/=0.0) and then (locx+locy+locz/= 0.0) then
            if DamageX>1 or else distance < 200.0 then
               -- Display_Warning.Warning(distance'Image);

               for i in 1..DamageX loop

                  if deflect in 80..100 then
                     case (Rand_Int.Random(gen)) is
                     when 1 => deflect := deflect - 1;
                     when others => null;
                     end case;
                     null;

                  elsif deflect in 70..79 then
                     case (Rand_Int.Random(gen)) is
                     when 1 => deflect := deflect - 1;
                     when 3 => hull := hull - 1;
                     when others => null;
                     end case;
                     null;
                  elsif deflect in 50..69 then
                     case (Rand_Int.Random(gen)) is
                     when 1 => deflect := deflect - 1;
                     when 2 => hull := hull - 1;
                     when 3 => deflect := deflect -1;
                     when others => null;
                     end case;
                  elsif deflect in 1..49 then
                     case (Rand_Int.Random(gen)) is
                     when 1 => deflect := deflect - 1;
                     when 2 => hull := hull - 1;
                     when 3 => deflect := deflect - 1;
                     when 4 => hull := hull - 1;
                     when 5 => hull := hull - 1;
                     when others => null;
                     end case;
                  elsif deflect = 0 then
                     case (Rand_Int.Random(gen)) is
                     when 2 => jmpeng := jmpeng - 1;
                     when 4 => navcom := navcom - 1;
                     when 3 => engine := engine - 1;
                     when 1 => hull := hull - 1;
                     when 5 => hull := hull - 1;
                     when 6 => hull := hull - 1;
                     when others => null;

                     end case;


                  end if;

               end loop;



               SQLstatement := To_Unbounded_String("");
               SQLstatement := SQLstatement & "UPDATE ships SET deflect_funct =" & deflect'Image &
                 ", engine_funct =" &engine'Image& ",navcom_funct=" &navcom'Image&
                 ",jmpeng_funct=" &jmpeng'Image& ",hull_value=" &hull'Image& " WHERE ship_id = " & ShipID;

               scratch := To_Unbounded_String(Ada_Format.SPut ("%f ",F(Float(distance))));
               Damage_Report := Damage_Report & "Damage To Ship "& ShipID & " Range "& scratch &" : Deflector" & deflect'Image &
                 ", Engine" &engine'Image& ",Navcom" &navcom'Image&
                 ",Jump Engine" &jmpeng'Image& ",Hull" &hull'Image;

               Add (Standard_Window,
                    Line => 2,
                    Column => 1,
                    Str => To_String(Damage_Report));
               Clear_To_End_Of_Line;
             --  refresh;

               Stmt:= Prepare (To_String(SQLstatement));

               Dbase.DB_Guns.Execute(Stmt);
               Dbase.DB_Guns.Commit;

               if not Dbase.DB_Guns.Success then
                  Display_Warning.Warning("Panic Damage Failed",D => 2.0);
               end if;

              delay until Next;

            else
               Dbase.DB_Guns.Rollback;

               scratch := To_Unbounded_String(Ada_Format.SPut ("%f ",F(Float(distance))));

               Damage_Report := Damage_Report & "Out Of Range"& scratch & " Ship ID "& ShipID&" : Deflector" & deflect'Image &
                 ", Engine" &engine'Image& ",Navcom" &navcom'Image&
                 ",Jump Engine" &jmpeng'Image& ",Hull" &hull'Image;

               Add (Standard_Window,
                    Line => 2,
                    Column => 1,
                    Str => To_String(Damage_Report));
               Clear_To_End_Of_Line;
              -- refresh;


            end if;

         else
            Dbase.DB_Guns.Rollback;
            Display_Warning.Warning("No Firing at Midway",D => 3.0);

         end if;

      end if;



   end Inflict_Damage;

   function Pad (InStr : String;PadWdth : Integer) return String is
      padstr,tmpstr : Unbounded_String;
   begin
      tmpstr := To_Unbounded_String(SF.Trim(Instr,Ada.Strings.Left));
      if SU.Length(tmpstr) < PadWdth then
         for i in SU.Length(tmpstr) .. PadWdth-1 loop
            padstr := padstr & '0';
         end loop;
         return To_String(padstr) & To_String(tmpstr);
      else
         return To_String(tmpstr);
      end if;
   end Pad;

   function MkTimestamp (Now : Time) return String is
      Now_Year    : Year_Number;
      Now_Month   : Month_Number;
      Now_Day     : Day_Number;
    --  Now_Seconds : Day_Duration;
      Now_Hour : Hour_Number;
      Now_Minute: Minute_Number;
      Now_Second : Second_Number;
      Now_Sub_Second : Second_Duration;
      scratch : Unbounded_String;
   begin

       Split (Now,
                Now_Year,
                Now_Month,
                Now_Day,
                Now_Hour,
                Now_Minute,
                Now_Second,
                Now_Sub_Second
               );

         scratch := To_Unbounded_String(Now_Sub_Second'Image);

      return Ada_Format.SPut ("%s-%s-%s %s:%s:%s%s ",(F(Pad(Now_Year'Image,4)),F(Pad(Now_Month'Image,2)),F(Pad(Now_Day'Image,2)),
                              F(Pad(Now_Hour'Image,2)),F(Pad(Now_Minute'Image,2)),
                F(Pad(Now_Second'Image,2)),F(Slice(scratch,SU.Index(scratch,"."),SU.Length(scratch))     )));

   end MkTimestamp;

   function RdTimestamp (TimeStamp : String) return Time is
      Now_Year    : Year_Number;
      Now_Month   : Month_Number;
      Now_Day     : Day_Number;
      Now_Hour : Hour_Number;
      Now_Minute: Minute_Number;
      Now_Second : Second_Number;
      Now_Sub_Second : Second_Duration;
      scratch : Unbounded_String := To_Unbounded_String(TimeStamp);
      Cursor : Integer;

   begin



      Cursor := Index(scratch,"-");

      Now_Year := Year_Number'Value(Slice (Source => scratch,Low => 1,High => Cursor-1));

      Delete(scratch,1,Cursor);
      Cursor := Index(scratch,"-");

      Now_Month := Month_Number'Value(Slice (Source => scratch,Low => 1,High => Cursor-1));
      Delete(scratch,1,Cursor);
      Cursor := Index(scratch," ");

      Now_Day := Day_Number'Value(Slice (Source => scratch,Low => 1,High => Cursor-1));
      Delete(scratch,1,Cursor);

      Cursor := Index(scratch,":");
      Now_Hour := Hour_Number'Value(Slice (Source => scratch,Low => 1,High => Cursor-1));
      Delete(scratch,1,Cursor);

      Cursor := Index(scratch,":");
      Now_Minute := Minute_Number'Value(Slice (Source => scratch,Low => 1,High => Cursor-1));
      Delete(scratch,1,Cursor);

      Cursor := Index(scratch,".");
      Now_Second := Second_Number'Value(Slice (Source => scratch,Low => 1,High => Cursor-1));
      Now_Sub_Second := Second_Duration'Value(Slice(scratch,cursor,Length(scratch)));


       return Time_Of (Now_Year,
                Now_Month,
                Now_Day,
                Now_Hour,
                Now_Minute,
                Now_Second,
                Now_Sub_Second
               );



   end RdTimestamp;


   procedure Torpedo_Control (ShipID : Unbounded_String) is

      Width : Column_Position := 30;
      Length : Line_Position := 13;

      TermLnth : Line_Position;
      TermWdth : Column_Position;
      Display_Window : Window;
      c : Key_Code;
      Stmt : Prepared_Statement;
      CIB : Direct_Cursor;
      SQL, Damage_Report : Unbounded_String;
    --  navcom,jmpeng,engine,deflect,hull : Integer;
      locx,locy,locz,targx,targy,targz,distance,timett : Long_Long_Float;
      torplock : Integer;
      D    : Duration := 0.4;
      Now : Time := Clock;
      Next : Time := Now + D;

      task Torp_Animate is
         entry Start;
      end Torp_Animate;

      task body Torp_Animate is
         Now : Time := Clock;
         Timer : Time;
         Diff : Duration;
      begin
         accept Start;
         loop
            Now := Clock;
            Timer := Now + 1.0;
            Diff := Next - Now;
            Add (Display_Window,
                 Line => 6,
                 Column => 1,
                 Str => "Torp Running " & Image(Diff,Include_Time_Fraction => True) );
            Clear_To_End_Of_Line(Display_Window);
            refresh(Display_Window);
            delay until Timer;
         end loop;
      end Torp_Animate;





   begin

      Get_Size(Standard_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);

      if Width < TermWdth then

         Display_Window := Sub_Window(Win => Standard_Window,
                                      Number_Of_Lines => Length,
                                      Number_Of_Columns => Width,
                                      First_Line_Position => ((TermLnth - Length) / 2),
                                      First_Column_Position => ((TermWdth - Width) / 2) + 28);

         Clear(Display_Window);
         Box(Display_Window);
         Refresh(Display_Window);
      else
         Display_Warning.Warning("Terminal not wide enough");
      end if;

      SQL := SQL &
        "SELECT * FROM ships WHERE ship_id = " & ShipID &" FOR UPDATE";


      Stmt:= Prepare (To_String(SQL));

      CIB.Fetch (Dbase.DB_Guns, Stmt);

      if CIB.Has_Row then
       --  navcom := Integer'Value(Fld(CIB,"navcom_funct"));
       --  jmpeng := Integer'Value(Fld(CIB,"jmpeng_funct"));
       --  engine := Integer'Value(Fld(CIB,"engine_funct"));
       --  deflect:= Integer'Value(Fld(CIB,"deflect_funct"));
       --  hull   := Integer'Value(Fld(CIB,"hull_value"));
         torplock  := Integer'Value(Fld(CIB,"torp_lock"));
        -- Next := RdTimestamp(Fld(CIB,"created_on2"));

         locx := Dbase.MyLocX;
         locy := Dbase.MyLocY;
         locz := Dbase.MyLocZ;

         targx := Long_Long_Float'Value(Fld(CIB,"loc_x"));
         targy := Long_Long_Float'Value(Fld(CIB,"loc_y"));
         targz := Long_Long_Float'Value(Fld(CIB,"loc_z"));

         distance := Value_Functions.Sqrt(((locx - targx)**2) + ((locy-targy)**2) + ((locz-targz)**2));

         timett := ((distance / 50000.0)*60.0)*60.0;

         Add (Display_Window,
              Line => 1,
              Column => 1,
              Str => " Firing on Ship "& To_String(ShipID) );


         Add (Display_Window,
              Line => 2,
              Column => 1,
              Str => Ada_Format.SPut ("Rtt %f km",F(Float(distance))) );
         Add (Display_Window,
              Line => 3,
              Column => 1,
              Str => Ada_Format.SPut ("Ttt %f Secs",F(Float(timett))) );

         Now := Clock;
         Next := Now + Duration(timett);


         Add (Display_Window,
              Line => 4,
              Column => 1,
              Str =>  MkTimestamp(Next) );

       --  Add (Display_Window,
       --       Line => 6,
       --       Column => 1,
       --       Str =>  Image(Next) );
         Refresh(Display_Window);



         torplock := torplock + 1;

         SQL := To_Unbounded_String("");
         SQL := SQL & "UPDATE ships SET torp_time = '" & MkTimestamp(Next) & "' " &
         ", torp_lock = " & torplock'Image &
           " WHERE ship_id = " & ShipID;

         Stmt:= Prepare (To_String(SQL));

         Dbase.DB_Guns.Execute(Stmt);
         Dbase.DB_Guns.Commit;

         if not Dbase.DB_Guns.Success then
            Display_Warning.Warning("Torpedo Lock Failed",D => 2.0);

         else

            Add (Standard_Window,
                 Line => 2,
                 Column => 1,
                 Str => "" );
            Clear_To_End_Of_Line;
            refresh;

            Torp_Animate.Start;

            delay until Next;

            SQL := To_Unbounded_String("");
            SQL := SQL &
              "SELECT * FROM ships WHERE ship_id = " & ShipID;


            Stmt:= Prepare (To_String(SQL));

            CIB.Fetch (Dbase.DB_Guns, Stmt);

            if CIB.Has_Row then
               locx := Long_Long_Float'Value(Fld(CIB,"loc_x"));
               locy := Long_Long_Float'Value(Fld(CIB,"loc_y"));
               locz := Long_Long_Float'Value(Fld(CIB,"loc_z"));

               distance := Value_Functions.Sqrt(((locx - targx)**2) + ((locy-targy)**2) + ((locz-targz)**2));

               Now := Clock;
               Add (Display_Window,
                    Line => 7,
                    Column => 1,
                    Str => Ada_Format.SPut ("Torp Det %f km away",F(Float(distance))) );
               Add (Display_Window,
                    Line => 8,
                    Column => 1,
                    Str => "at " & Image(Now,True));

               refresh(Display_Window);
               if distance < 200.0 then
                  Inflict_Damage(ShipID,500);
               else
                Add (Display_Window,
                    Line => 9,
                    Column => 1,
                     Str => "-No Damage-");
                  refresh(Display_Window);
               end if;

            end if;

         end if;

      end if;










      Abort Torp_Animate;

      c := Texaco.GetKey;

      Clear(Display_Window);
      Refresh(Display_Window);

      Delete (Win => Display_Window);
   end Torpedo_Control;



   type Status_Record is record
      Prompt : String_Access;
      FieldName : String_Access;
      Blinking :  Boolean;
      SaveValue : Unbounded_String;
   end record;

   type Status_Field_Type is array (Positive range <>) of Status_Record;

   Status_Field_List : Status_Field_Type  :=
     ((new String'("NavCom"),new String'("navcom_funct"),False,To_Unbounded_String("")),
      (new String'("JmpEng"),new String'("jmpeng_funct"),False,To_Unbounded_String("")),
      (new String'("Engine"),new String'("engine_funct"),False,To_Unbounded_String("")),
      (new String'("Dflctr"),new String'("deflect_funct"),False,To_Unbounded_String("")),
      (new String'("Hull"),new String'("hull_value"),False,To_Unbounded_String(""))
     );

   procedure Update_Status is
      procedure Blink(fldnm : String) is
      begin
         for i in Status_Field_List'Range loop
            if Status_Field_List(i).Prompt.all = fldnm then
               Status_Field_List(i).Blinking := True;
            end if;
         end loop;


      end Blink;

   begin

      for i in Status_Field_List'Range loop


         if Status_Field_List(i).SaveValue /=
           Current_Record(To_Unbounded_String(Status_Field_List(i).FieldName.all)) then
            if Status_Field_List(i).Blinking then
               -- Status_Field_List(i).Blinking := False;
               Status_Field_List(i).SaveValue :=
                 Current_Record(To_Unbounded_String(Status_Field_List(i).FieldName.all));
            else
               Status_Field_List(i).Blinking := True;
            end if;
         else
            Status_Field_List(i).Blinking := False;
         end if;


         if Status_Field_List(i).Blinking then
            Set_Character_Attributes(Standard_Window, (Reverse_Video => True,Blink => True,others => False));
         else
            Set_Character_Attributes(Standard_Window, Normal_Video);
         end if;


         Add (Standard_Window,
              Line => 1,
              Column => Column_Position(i * 8),
              Str => Status_Field_List(i).Prompt.all);

         refresh;

      end loop;
      Set_Character_Attributes(Standard_Window, Normal_Video);
      refresh;

   end Update_Status;

   procedure Fire_Lasers (Ship_ID : Unbounded_String) is

   begin
      if Firing_Queue.Current_Use < 10 then
         Firing_Queue.Enqueue(New_Item => Ship_ID);
      end if;
   end Fire_Lasers;

   TorpLockSave : Unbounded_String;
   procedure torpedo_process is
      Torp_Impact_Time : Time;
      TorpImpactTimestamp,torp_lock : Unbounded_String;

      task Torp_Animate is
         entry Start;
      end Torp_Animate;

      task body Torp_Animate is
         Now : Time := Clock;
         Next : Time;
         Diff : Duration;
      begin
          accept Start;
         loop
            Now := Clock;
            Next := Now + 1.0;
            Diff := Torp_Impact_Time - Now;
            Set_Character_Attributes(Standard_Window, (Reverse_Video => True,Blink => True,others => False));
            Add (Standard_Window,
                 Line => 3,
                 Column => 1,
                 Str => "Incoming Torpedo impact in ");
            Set_Character_Attributes(Standard_Window, Normal_Video);
            Add (Standard_Window,
                 Line => 3,
                 Column => 28,
                 Str => Image(Diff,Include_Time_Fraction => True) );

            Clear_To_End_Of_Line;
           -- refresh;
            delay until Next;
         end loop;
      end Torp_Animate;


   begin
      torp_lock := Current_Record(To_Unbounded_String("torp_lock"));

      if Length(TorpLockSave) = 0 then
         TorpLockSave := torp_lock;
      else
         if TorpLockSave /= torp_lock then
            TorpLockSave := torp_lock;
            TorpImpactTimestamp := Current_Record(To_Unbounded_String("torp_time"));

            Torp_Impact_Time := RdTimestamp(To_String(TorpImpactTimestamp));

            Torp_Animate.Start;

            delay until Torp_Impact_Time;
            Abort Torp_Animate;
            Torp_Impact_Time := Clock;

            Add (Standard_Window,
                 Line => 3,
                 Column => 1,
                 Str => "Torpedo Exploding at " & Image(Torp_Impact_Time,True));
            Clear_To_End_Of_Line;
            refresh;
            -- Display_Warning.Warning("Torpedo Exploded On Ship",D => 3.0);

         end if;
      end if;

      Abort Torp_Animate;

   end torpedo_process;


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
     ((new String'("Radar Scope"),Radar_Scan'Unrestricted_Access),
      (new String'("Null"),dummy'Unrestricted_Access));


   procedure Command_Screen is
      c : Key_Code;
      StopOverwrite : Boolean := False;
    --  StopTorp :Boolean := False;


      task Background_Processor is
         entry Start;
      end Background_Processor;

      task Firing_Processor;
      task Torpedo_Processor;

      task body Background_Processor is
         Next : Time;
         D    : Duration := 1.0;
         Now : Time; -- := Clock;
         Clock_Window : Window;

      begin
         accept Start;
         Clock_Window := Sub_Window(Win => Standard_Window,
                         Number_Of_Lines => 1,
                         Number_Of_Columns => 32,
                         First_Line_Position => 1,
                         First_Column_Position => 70);
 --     -- Box(win1);
 --     Refresh(Win => win1);

         loop
            Now := Clock;
            Next := Now + D;
            -- Add (Win => Clock_Window,Line => 1,Column => 70,Str => Image (Now));
            Add (Win => Clock_Window,Line => 0,Column => 0,Str => Image (Now));
            Refresh(Clock_Window);

            Recycle;

            Update_Status;

            if not StopOverwrite then
             --  Recycle;
               Redraw_Page;
           -- else
           --    Refresh(Display_Window);
            end if;

            delay until Next;
         end loop;
      end Background_Processor;

      task body Firing_Processor is
         Ship_ID : Unbounded_String;
      begin

         loop
            Firing_Queue.Dequeue(Element => Ship_ID);
            Inflict_Damage(Ship_ID);
         end loop;

      end Firing_Processor;

      task body Torpedo_Processor is
         Next : Time;
         D    : Duration := 1.0;
         Now : Time; -- := Clock;
      begin
         loop
            Now := Clock;
            Next := Now + D;
            torpedo_process;

            delay until Next;


         end loop;

      end Torpedo_Processor;

   begin




      Add (Line => Lines - 2,Column => 1, Str => "1 Navig  |2 Engine |3 Radar  |4 Weapons|");
      Refresh;
      Background_Processor.Start;
      loop
         Redraw_Page;
         StopOverwrite := False;
         c := Texaco.GetKey;  -- Get_Keystroke;
         StopOverwrite := True;
         Nap_Milli_Seconds(200);
         if c in Special_Key_Code'Range then
            case c is
            when Key_F1 =>

               Process_Menu.Open_Menu (Function_Number => 1,Menu_Array => Navigation );

            when Key_F2 =>
               Process_Menu.Open_Menu (Function_Number => 2,Menu_Array => Navigation);
            when Key_F3 =>
               Radar_Scan;
               -- Process_Menu.Open_Menu (Function_Number => 3,Menu_Array => Radar);
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
      Abort Background_Processor;
      abort Firing_Processor;
      abort Torpedo_Processor;
   exception
     when CONSTRAINT_ERROR => null;
         Abort Background_Processor;
         abort Firing_Processor;
         abort Torpedo_Processor;
   end Command_Screen;

begin
   Rand_Int.Reset(gen);

end Templates;
