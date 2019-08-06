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

   procedure Read_Header (FileName : in String;
                          Sender : out Unbounded_String;
                          Subject : out Unbounded_String;
                          Msgid : out Unbounded_String;
                          ReplyTo : out Unbounded_String) is
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
         if HeaderType = "Sender" then
            Sender := HeaderText;
         elsif HeaderType = "Subject" then
            Subject := HeaderText;
         elsif HeaderType = "Msgid" then
            Msgid := HeaderText;
         elsif HeaderType = "ReplyTo" then
            ReplyTo := HeaderText;
         end if;
         scratch := SUIO.Get_Line(File);
      end loop;
      Close (File);
   exception
      when End_Error =>
         Close (File);
         null;
   end Read_Header;


   procedure Read_Directory is
      Dir : Directory_Entry_Type;
      Dir_Search : Search_Type;

      -- Curr_Dir : string := Current_Directory;

      Sender, Subject,Msgid,ReplyTo : Unbounded_String;
      I, J, SortCurs : Cursor;
      swapped : Boolean;

   begin
      Clear(Directory_Buffer);

      Start_Search(Search => Dir_Search,
                   Directory => Curr_Dir&"/messages",
                   Pattern => "*.msg");
      loop
         Get_Next_Entry(Dir_Search, Dir);

         Read_Header(Full_Name(Dir),Sender  => Sender,
                     Subject => Subject,Msgid => Msgid,ReplyTo => ReplyTo);

         Directory_Buffer.Append(New_Item => (To_Unbounded_String(Full_Name(Dir)),
                                              Sender & Character'Val (9) & Subject) );

         exit when not More_Entries(Dir_Search);
      end loop;

      End_Search(Dir_Search);

      -- Bubble Sort Director Buffer
      loop
         SortCurs := Directory_Buffer.First;
         swapped := False;
         while SortCurs /= Directory_Buffer.Last loop
            I := SortCurs;
            Directory_List.Next(SortCurs);
            J := SortCurs;
            if Element(J).FileName < Element(I).FileName then
               Swap(Directory_Buffer,I,J);
               swapped := True;
            end if;
         end loop;
         exit when not swapped;
      end loop;

   end Read_Directory;

   function ReRead_Directory return Boolean is
   begin
      Read_Directory;

      return True;
   end ReRead_Directory;

   function Post_Reply return Boolean is
   begin
      Display_Warning.Warning("Replys  Not Implemented yet");
      return True;
   end;


   MessageMenu : Process_Menu.Menu_Type  :=
     ((new String'("Post Message"),Post_Message'Access),
      (new String'("Post Reply"),Post_Reply'Access),
     (new String'("Reload Msgs"),ReRead_Directory'Access));



   procedure Read_Messages is
      curs : Cursor;
      c : Key_Code;

      procedure Redraw_Screen is
         curs2 : Cursor;
         LineNum : Line_Position := 0;
      begin
         if not Directory_Buffer.Is_Empty then
            curs2 := curs;
            for i in 1 .. CurrentLine loop
               if curs2 /= Directory_Buffer.First then
                  Directory_List.Previous(curs2);
               end if;
            end loop;
            while curs2 /= Directory_Buffer.Last loop

               Add(Standard_Window,Line => TopLine + LineNum,Column => 0,Str => To_String(Element(curs2).Prompt) );
               Clear_To_End_Of_Line;
               Refresh;

               Directory_List.Next(curs2);

               LineNum := LineNum +1;
               exit when LineNum+ TopLine >= BottomLine;
            end loop;
            Add(Standard_Window,Line => TopLine + LineNum,Column => 0,Str => To_String(Element(curs2).Prompt) );
            Clear_To_End_Of_Line;
            Refresh;
         end if;
      end Redraw_Screen;

      FindElement : Directory_Record;
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
         Add (Line => TermLnth - 2,Column => 1, Str => " Func 1  |                        Esc exit");
         Clear_To_End_Of_Line;
         Refresh;

         HiLite(Standard_Window,Element(curs).Prompt,CurrentLine+TopLine);

         c := Get_Keystroke;

         if c in Special_Key_Code'Range then
            case c is
            when Key_F1 =>
               FindElement := Element(curs);
               Process_Menu.Open_Menu (Function_Number => 1,Menu_Array => MessageMenu );
               curs := Directory_Buffer.Find(Item => FindElement);
               Clear;
               Redraw_Screen;
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

   function Post_Message return Boolean is
      FileName : Unbounded_String := To_Unbounded_String("messages/test.txt");
      File : File_Type;
      Nick, Subject, Msgid, FName : Unbounded_String;

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


      function Generate_UID return Unbounded_String is
         Now         : Time := Clock;
         Now_Year    : Year_Number;
         Now_Month   : Month_Number;
         Now_Day     : Day_Number;
         Now_Seconds : Day_Duration;
      begin
         Split (Now,
                Now_Year,
                Now_Month,
                Now_Day,
                Now_Seconds);

         return To_Unbounded_String(SF.Trim(Year_Number'Image (Now_Year),Ada.Strings.Left) &
                                      Pad(Month_Number'Image (Now_Month),2) &
                                      Pad(Day_Number'Image (Now_Day),2) &
                                      Pad(Duration'Image (Now_Seconds),16));

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

            FName := Msgid & ".msg";
            Create (File => File,
                    Mode => Out_File,
                    Name => To_String("messages/" & FName));

            SUIO.Put_Line(File,"Sender: " & Nick);
            SUIO.Put_Line(File,"Subject: " & Subject);
            SUIO.Put_Line(File,"Msgid: " & Msgid);
            SUIO.Put_Line(File,To_Unbounded_String(""));

            Text_Buffer.Iterate(Write_Line'access);

            Close (File);

            Directory_Buffer.Append(New_Item => (To_Unbounded_String(Curr_Dir&"/messages/"&To_String(FName)),
                                              Nick & Character'Val (9) & Subject) );

         else
            Add (Line => 3,Column => 0, Str => "Cancelling Message");
            Clear_To_End_Of_Line;
            Refresh;
         end if;

      end if;

      return True;
   end Post_Message;



end Message.Reader;
