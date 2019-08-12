with Text_File_Scroller;
with Texaco; use Texaco;
with Message.Post;

package body Message.Reader is

 --  CurrentLine : Line_Position := 0;
 --  CurrentCurs : Cursor;
 --  TopLine : Line_Position;
 --  TermLnth : Line_Position;
 --  TermWdth : Column_Position;
  -- BottomLine : Line_Position ;

 --  Curr_Dir : string := Current_Directory;

   procedure Scroll_Up is
   begin
      Move_Cursor(Line   => TopLine,Column => 0);
      Delete_Line;
      Move_Cursor(Line   => BottomLine,Column => 0);
      Insert_Line;
      Box;
      Refresh;
   end Scroll_Up;

   procedure Scroll_Down is
   begin
      Move_Cursor(Line   => BottomLine,Column => 0);
      Delete_Line;
      Move_Cursor(Line   => TopLine,Column => 0);
      Insert_Line;
      Box;
      Refresh;
   end Scroll_Down;

   Procedure Clear_Region is
   begin
      for i in TopLine .. BottomLine loop
         Move_Cursor(Line   => i,Column => 2);
         Clear_To_End_Of_Line;
      end loop;
      -- CurrentLine := 0;
   end Clear_Region;



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
           Column => 2,
           Str => To_String(Prompt));
      Refresh(Win);
      Set_Character_Attributes(Win, Normal_Video);
   end HiLite;

   procedure LoLite (Win : Window; Prompt : Unbounded_String; Line_Num : Line_Position) is
   begin
      Set_Character_Attributes(Win, Normal_Video);
      Add (Win => Win,
           Line => Line_Num,
           Column => 2,
           Str => To_String(Prompt));
      Refresh(Win);
   end LoLite;

   procedure Redraw_Screen is
      curs2 : Cursor;
      LineNum : Line_Position := 0;
   begin
      Clear_Region;
      if not Directory_Buffer.Is_Empty then
         curs2 := CurrentCurs;
         for i in 1 .. CurrentLine loop
            if curs2 /= Directory_Buffer.First then
               Directory_List.Previous(curs2);
            end if;
         end loop;
         while curs2 /= Directory_Buffer.Last loop

            Add(Standard_Window,Line => TopLine + LineNum,Column => 2,Str => To_String(Element(curs2).Prompt) );
            Clear_To_End_Of_Line;
            Refresh;

            Directory_List.Next(curs2);

            LineNum := LineNum +1;
            exit when LineNum+ TopLine >= BottomLine;
         end loop;
         Add(Standard_Window,Line => TopLine + LineNum,Column => 2,Str => To_String(Element(curs2).Prompt) );
         Clear_To_End_Of_Line;
         Refresh;

         Add (Line => TermLnth - 2,Column => 1, Str => "          | Func 2  |                        End to exit");
         Clear_To_End_Of_Line;
         Box;
      end if;
   end Redraw_Screen;


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

   function CharPad(InStr : Unbounded_String; PadWidth : Integer) return String is
      padstr : Unbounded_String;
   begin
      if SU.Length(InStr) <= PadWidth then
         for i in SU.Length(InStr) .. PadWidth loop
            padstr := padstr & " ";
         end loop;
      end if;

      return To_String(Instr & padstr);

   end CharPad;


   procedure Read_Directory (ReplyID : Unbounded_String := To_Unbounded_String("")) is
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
         ReplyTo := To_Unbounded_String("");
         Msgid := To_Unbounded_String("");
         Read_Header(Full_Name(Dir),Sender  => Sender,
                     Subject => Subject,Msgid => Msgid,ReplyTo => ReplyTo);

         if SU.Length(ReplyID) > 0   then

            if ReplyTo = ReplyID or else Msgid = ReplyID then
               Directory_Buffer.Append(New_Item => (To_Unbounded_String(Full_Name(Dir)),
                                                    CharPad(Sender,15) & Subject) );
            end if;

         else
             Directory_Buffer.Append(New_Item => (To_Unbounded_String(Full_Name(Dir)),
                                                  CharPad(Sender,15) & Subject) );
         end if;

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

   procedure ReRead_Directory is
   begin
      Read_Directory;
   end ReRead_Directory;

   procedure Post_Reply is
      Sender, Subject, MsgId, ReplyTo : Unbounded_String;
   begin

      Read_Header(To_String(Element(CurrentCurs).FileName) ,Sender  => Sender,
                  Subject => Subject,Msgid => Msgid,ReplyTo => ReplyTo);

      Subject := "Re. " & Subject;

      Message.Post.Quote(Msgid => Msgid);
      Message.Post.Post_Message(MsgId,Subject);

   end;

   procedure Post_Thread_Reply is
      Sender, Subject, MsgId, ReplyTo : Unbounded_String;
   begin

       Read_Header(To_String(Element(CurrentCurs).FileName) ,Sender  => Sender,
                   Subject => Subject,Msgid => Msgid,ReplyTo => ReplyTo);
      if SU.Length(ReplyTo) > 0 then
         Message.Post.Quote(Msgid => Msgid);
         Message.Post.Post_Message(ReplyTo,Subject);
      else
         Display_Warning.Warning("Selected message not part of a thread");

      end if;

   end Post_Thread_Reply;



   procedure Show_Thread is
      Sender, Subject, MsgId, ReplyTo : Unbounded_String;
      DefaultLength : Ada.Containers.Count_Type := 1;
   begin

       Read_Header(To_String(Element(CurrentCurs).FileName) ,Sender  => Sender,
                   Subject => Subject,Msgid => Msgid,ReplyTo => ReplyTo);

      if SU.Length(ReplyTo) = 0 then

         Read_Directory(ReplyID => MsgId);
         if Directory_Buffer.Length = DefaultLength then
            Display_Warning.Warning("No Replys to this message");
            Read_Directory;
         end if;
         if CurrentLine > Line_Position(Directory_Buffer.Length-1) then
            CurrentLine := Line_Position(Directory_Buffer.Length-1);
         end if;

       --  CurrentLine := 0;
       --  CurrentCurs := Directory_Buffer.First;
      else
         Read_Directory(ReplyID => ReplyTo);
         if CurrentLine > Line_Position(Directory_Buffer.Length) then
            -- CurrentLine := 0; -- Line_Position(Directory_Buffer.Length);
            -- CurrentCurs := Directory_Buffer.Last;
            null;
         end if;
       --  CurrentLine := 0;
       --  CurrentCurs := Directory_Buffer.First;
      end if;

   end Show_Thread;


   procedure Run_Post_Message is
   begin
      Text_Buffer.Clear;
      Message.Post.Post_Message;


   end Run_Post_Message;


   MessageMenu : Process_Menu.Menu_Type  :=
     ((new String'("Show Reply Thread"),Show_Thread'Access),
      (new String'("Reply To Thread"),Post_Thread_Reply'Access),
      (new String'("Reply To Message"),Post_Reply'Access),
      (new String'("Post New Message"),Run_Post_Message'Access),
      (new String'("Reload Messages"),ReRead_Directory'Access),
      (new String'("User Login"),Message.Login.Login_User'Access),
      (new String'("Create User"),Message.Login.Create_User'Access));




   procedure Read_Messages is
      c : Key_Code;
      FindElement : Directory_Record;

   begin

      Clear;

      Get_Size(Standard_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
      TopLine := 4;
      BottomLine := TermLnth - 4;
      CurrentLine := 0;

      Read_Directory;

      CurrentCurs := Directory_Buffer.First;

      Redraw_Screen;

      Refresh;

      loop

         HiLite(Standard_Window,Element(CurrentCurs).Prompt,CurrentLine+TopLine);

         c := Get_Keystroke;

         if c in Special_Key_Code'Range then
            case c is
            when Key_F2 =>
               FindElement := Element(CurrentCurs);
               Process_Menu.Open_Menu (Function_Number => 2,Menu_Array => MessageMenu );
               CurrentCurs := Directory_Buffer.Find(Item => FindElement);
               if CurrentCurs = No_Element then
                  CurrentCurs := Directory_Buffer.Last;
               end if;

               -- Try to make CurrentLine right for repositioned CurrentCurs
               if Line_Position(Directory_Buffer.Length) < BottomLine-TopLine then
                  declare
                     CountCurs : Cursor := Directory_Buffer.First;
                     Counter : Integer := 0;
                  begin
                     while CountCurs /= Directory_Buffer.Last loop

                        exit when CountCurs = CurrentCurs;
                        Counter := Counter +1;
                        CountCurs := Directory_List.Next(CountCurs);

                     end loop;
                     CurrentLine := Line_Position(Counter);
                  end;
               end if;

               Clear;
               Redraw_Screen;
            when Key_Cursor_Down =>
               if (CurrentCurs /= Directory_Buffer.Last) then
                  LoLite(Standard_Window,Element(CurrentCurs).Prompt,CurrentLine+TopLine);
                  Increment(CurrentLine);
                  Directory_List.Next(CurrentCurs);
               end if;
            when Key_Cursor_Up =>
               if (CurrentCurs /= Directory_Buffer.First) then
                  LoLite(Standard_Window,Element(CurrentCurs).Prompt,CurrentLine+TopLine);
                  Decrement(CurrentLine);
                  Directory_List.Previous(CurrentCurs);
               end if;
            when Key_Resize =>
               Get_Size(Standard_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
               BottomLine := TermLnth - 4;
               Clear;
               Redraw_Screen;
            when Key_End => exit;
            when others => null;
            end case;
         elsif c in Real_Key_Code'Range then

            -- Ch := Character'Val (c);

            case Character'Val (c) is
            when LF | CR =>
               begin
                  if Exists(To_String(Element(CurrentCurs).FileName)) then
                     Text_File_Scroller(To_String(Element(CurrentCurs).FileName));
                     Redraw_Screen;
                  else
                     Display_Warning.Warning("Message Has been deleted");
                  end if;

               end;
            when ESC => Null;
            when others => null;
            end case;
         end if;

      end loop;

   end Read_Messages;



end Message.Reader;
