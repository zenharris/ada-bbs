with Text_File_Scroller;
with Texaco; use Texaco;

package body Message.Reader is

   CurrentLine : Line_Position := 0;
   TopLine : Line_Position;
   TermLnth : Line_Position;
   TermWdth : Column_Position;
   BottomLine : Line_Position ;

   Curr_Dir : string := Current_Directory;

    procedure Scroll_Up is
      begin
         Move_Cursor(Line   => TopLine,Column => 0);
         Delete_Line;
         Move_Cursor(Line   => BottomLine,Column => 0);
         Insert_Line;
         Refresh;
      end Scroll_Up;

      procedure Scroll_Down is
      begin
         Move_Cursor(Line   => BottomLine,Column => 0);
         Delete_Line;
         Move_Cursor(Line   => TopLine,Column => 0);
         Insert_Line;
         Refresh;
      end Scroll_Down;

   procedure Increment (IncLine : in out Line_Position) is
   begin
      if TopLine + Incline < BottomLine then
         IncLine := IncLine + 1;
      else
         Scroll_Up;
      end if;
   end Increment;

   procedure Decrement (IncLine : in out Line_Position) is
   begin
      if Incline > 0 then
         IncLine := IncLine - 1;
      else
         Scroll_Down;
      end if;
   end Decrement;

   procedure HiLite (Win : Window; Prompt : Unbounded_String; Line_Num : Line_Position) is
   begin
      Set_Character_Attributes(Win, (Reverse_Video => True,others => False));
      Add (Win => Win,
           Line => Line_Num,
           Column => 0,
           Str => To_String(Prompt));
      Refresh(Win);
      Set_Character_Attributes(Win, Normal_Video);
   end HiLite;

   procedure LoLite (Win : Window; Prompt : Unbounded_String; Line_Num : Line_Position) is
   begin
      Set_Character_Attributes(Win, Normal_Video);
      Add (Win => Win,
           Line => Line_Num,
           Column => 0,
           Str => To_String(Prompt));
      Refresh(Win);
   end LoLite;


   procedure Read_Directory is
      Dir : Directory_Entry_Type;
      Dir_Search : Search_Type;

      -- Curr_Dir : string := Current_Directory;
      File : File_Type;

      HeaderType, HeaderText, Sender, Subject, scratch : Unbounded_String;
      I, J, curs : Cursor;
      swapped : Boolean;

   begin
      Clear(Directory_Buffer);

      Start_Search(Search => Dir_Search,
                   Directory => Curr_Dir&"/messages",
                   Pattern => "*.msg");
      loop
         Get_Next_Entry(Dir_Search, Dir);


         Open (File => File,
               Mode => In_File,
               Name => Full_Name(Dir));

         While not  End_Of_File (File) Loop

            scratch := SUIO.Get_Line(File);
            while scratch /= "" loop
               HeaderType := To_Unbounded_String(SU.Slice(scratch,1,SU.Index(scratch,":")-1));
               HeaderText := To_Unbounded_String(SU.Slice(scratch,SU.Index(scratch,":")+2,SU.Length(scratch)));
               if HeaderType = "Sender" then
                  Sender := HeaderText;
               elsif HeaderType = "Subject" then
                  Subject := HeaderText;
               end if;
               scratch := SUIO.Get_Line(File);
            end loop;

            Directory_Buffer.Append(New_Item => (To_Unbounded_String(Full_Name(Dir)),
                                                 Sender & Character'Val (9) & Subject) );

            exit;

         end loop;

         Close (File);

         exit when not More_Entries(Dir_Search);
      end loop;

      End_Search(Dir_Search);


      -- Bubble Sort Director Buffer
      loop
         curs := Directory_Buffer.First;
         swapped := False;
         while curs /= Directory_Buffer.Last loop
            I := curs;
            Directory_List.Next(curs);
            J := curs;
            if Element(J).FileName < Element(I).FileName then
               Swap(Directory_Buffer,I,J);
               swapped := True;
            end if;
         end loop;
         exit when not swapped;
      end loop;

   end Read_Directory;




   procedure Read_Messages is
      curs : Cursor;
      c : Key_Code;

      procedure Redraw_Screen is
         curs2 : Cursor;
         LineNum : Line_Position := 0;
      begin
         if not Directory_Buffer.Is_Empty then
            curs2 := curs;
            for i in 0 .. CurrentLine loop
               if curs2 /= Directory_Buffer.First then
                  Directory_List.Previous(curs2);
               end if;
            end loop;
            while curs2 /= Directory_Buffer.Last loop

               Add(Standard_Window,Line => TopLine + LineNum,Column => 0,Str => To_String(Element(curs2).Prompt) );
               Clear_To_End_Of_Line;
               Refresh;
               LineNum := LineNum +1;
               exit when LineNum+ TopLine >= BottomLine;
               Directory_List.Next(curs2);

            end loop;
            Add(Standard_Window,Line => TopLine + LineNum,Column => 0,Str => To_String(Element(curs2).Prompt) );
            Refresh;
         end if;
      end Redraw_Screen;

   begin
      Clear;

      Get_Size(Standard_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
      TopLine := 4;
      BottomLine := TermLnth - 4;
      CurrentLine := 0;
      Read_Directory;

      curs := Directory_Buffer.First;

      Redraw_Screen;

      loop
         HiLite(Standard_Window,Element(curs).Prompt,CurrentLine+TopLine);

         c := Get_Keystroke;

         if c in Special_Key_Code'Range then
            case c is
            when Key_Cursor_Down =>
               if (curs /= Directory_Buffer.Last) then
                  LoLite(Standard_Window,Element(curs).Prompt,CurrentLine+TopLine);
                  Increment(CurrentLine);
                  Directory_List.Next(curs);
               end if;
            when Key_Cursor_Up =>
               if (curs /= Directory_Buffer.First) then
                  -- LoLite(menu_win,Menu_Array,Current_Line);
                  LoLite(Standard_Window,Element(curs).Prompt,CurrentLine+TopLine);
                  Decrement(CurrentLine);
                  Directory_List.Previous(curs);
               end if;
            when others => null;
            end case;
         elsif c in Real_Key_Code'Range then

            -- Ch := Character'Val (c);

            case Character'Val (c) is
            when LF | CR =>
               begin

                  Text_File_Scroller(To_String(Element(curs).FileName));
                 Redraw_Screen;
               end;
            when ESC =>
               begin
                  exit;
               end;
            when others => null;
            end case;
         end if;

      end loop;

   end Read_Messages;

   procedure Post_Message is
      FileName : Unbounded_String := To_Unbounded_String("messages/test.txt");
      File : File_Type;
      Nick, Subject, Msgid : Unbounded_String;



      function Generate_UID return Unbounded_String is
         Now         : Time := Clock;
         Now_Year    : Year_Number;
         Now_Month   : Month_Number;
         Now_Day     : Day_Number;
         Now_Seconds : Day_Duration;
         DayPad, MonthPad : Unbounded_String;

      begin
         Split (Now,
                Now_Year,
                Now_Month,
                Now_Day,
                Now_Seconds);

         if (Now_Day < 10) then
            DayPad := To_Unbounded_String("0"& SF.Trim(Day_Number'Image (Now_Day),Ada.Strings.Left));
         else
            DayPad := To_Unbounded_String(SF.Trim(Day_Number'Image (Now_Day),Ada.Strings.Left));
         end if;
         if (Now_Month < 10) then
            MonthPad := To_Unbounded_String("0"& SF.Trim(Month_Number'Image (Now_Month),Ada.Strings.Left));
         else
            MonthPad := To_Unbounded_String(SF.Trim(Month_Number'Image (Now_Month),Ada.Strings.Left));
         end if;


       return To_Unbounded_String(SF.Trim(Year_Number'Image (Now_Year),Ada.Strings.Left) &
           To_String(MonthPad) &
           To_String(DayPad) &
           SF.Trim(Duration'Image (Now_Seconds),Ada.Strings.Left));

      end Generate_UID;


      procedure Write_Line (Position : String_List.Cursor) is
      begin

         SUIO.Put_Line(File,String_List.Element(Position));

      end Write_Line;

   begin
      Clear;


      Get_Size(Standard_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
      TopLine := 4;
      BottomLine := TermLnth - 4;

      Add (Line => 1,Column => 0,Str => "Nick : ");
      Nick := To_Unbounded_String("");
      loop
         Texaco.Line_Editor(Standard_Window,
                            StartLine => 1,
                            StartColumn => 7,
                            Editlength => 20,
                            Edline => Nick,
                            MaxLength => 20);
         exit when Nick /= "";
      end loop;

      Add (Line => 2,Column => 0,Str => "Subject : ");
      Subject := To_Unbounded_String("");
      loop
         Texaco.Line_Editor(Standard_Window,
                            StartLine => 2,
                            StartColumn => 11,
                            Editlength => 60,
                            Edline => Subject,
                            MaxLength => 60);
         exit when Subject /= "";
      end loop;
      Add (Line => 3,Column => 0, Str => "Enter your Message text. Esc to exit");
      Refresh;


      Texaco.Text_Editor(Standard_Window,TopLine => TopLine,BottomLine => BottomLine,MaxLines => 100);



      if not Text_Buffer.Is_Empty then
        if Display_Warning.GetYN("Do you want to save this message Y/N") then
            Add (Line => 3,Column => 0, Str => "Posting Message");
            Clear_To_End_Of_Line;
            Refresh;
            Msgid := Generate_UID;


            Create (File => File,
                    Mode => Out_File,
                    Name => To_String("messages/" & Msgid & ".msg"));

            SUIO.Put_Line(File,"Sender: " & Nick);
            SUIO.Put_Line(File,"Subject: " & Subject);
            SUIO.Put_Line(File,"Msgid: " & Msgid);
            SUIO.Put_Line(File,To_Unbounded_String(""));

            Text_Buffer.Iterate(Write_Line'access);

            Close (File);
         else
            Add (Line => 3,Column => 0, Str => "Cancelling Message");
            Clear_To_End_Of_Line;
            Refresh;
         end if;

      end if;


   end Post_Message;






end Message.Reader;
