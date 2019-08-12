with Message.Reader; use Message.Reader;

package body Message.Post is

   procedure Quote (Msgid : Unbounded_String) is

      FileName, scratch : Unbounded_String;
      File : File_Type;
      Sender, HeaderType,HeaderText : Unbounded_String;
   begin
      FileName := "messages/"& Msgid &".msg";

     -- Message.Reader.Read_Header(To_String(FileName),Sender  => Sender,
     --                Subject => Subject,Msgid => MsgidTmp,ReplyTo => ReplyTo);

      Open (File => File,
            Mode => In_File,
            Name => To_String(Filename));

      Text_Buffer.Clear;


      scratch := SUIO.Get_Line(File);
      while scratch /= "" loop
         HeaderType := To_Unbounded_String(SU.Slice(scratch,1,SU.Index(scratch,":")-1));
         HeaderText := To_Unbounded_String(SU.Slice(scratch,SU.Index(scratch,":")+2,SU.Length(scratch)));
         if HeaderType = "Sender" then
            Sender := HeaderText;
            end if;
         scratch := SUIO.Get_Line(File);
      end loop;

      Text_Buffer.Append("In reply to "& Sender);

      scratch := SUIO.Get_Line(File);
      loop
         Text_Buffer.Append(" > "& scratch);
         scratch := SUIO.Get_Line(File);
      end loop;
   --   Close (File);

   exception
      when End_Error =>
         Close (File);

   end Quote;




  procedure Post_Message (ReplyID : in Unbounded_String := To_Unbounded_String("");
                          ReplySubject : in Unbounded_String := To_Unbounded_String("")) is
      -- FileName : Unbounded_String := To_Unbounded_String("messages/test.txt");
      File : File_Type;
      Nick, Subject, Msgid, FName : Unbounded_String;
      PostDate : Time := Clock;
      Cancelled : Boolean := False;

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

      if UserLoggedIn then

         Clear;

         Get_Size(Standard_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
         TopLine := 4;
         BottomLine := TermLnth - 4;

         Nick := UserLoggedName;

         Add (Line => 1,Column => 0,Str => "Nick : " & To_String(Nick));


         if (SU.Length(ReplySubject) > 0) then

            if SU.Index(ReplySubject,"Re.") = 1 then
               Subject := ReplySubject;
            else
               Subject := "Re. " & ReplySubject;
            end if;

         end if;

         Add (Line => 2,Column => 0,Str => "Subject : ");
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
            if Texaco.Save then

               Msgid := Generate_UID;

               FName := "messages/" & Msgid & ".msg";
               Create (File => File,
                       Mode => Out_File,
                       Name => To_String(FName));

               SUIO.Put_Line(File,"Sender: " & Nick);
               SUIO.Put_Line(File,"Subject: " & Subject);
               SUIO.Put_Line(File,"PostDate: " & To_Unbounded_String(Image (PostDate)));
               SUIO.Put_Line(File,"Msgid: " & Msgid);

               if SU.Length(ReplyID) /= 0 then
                  SUIO.Put_Line(File,"ReplyTo: " & ReplyID);
               end if;

               SUIO.Put_Line(File,To_Unbounded_String(""));

               Text_Buffer.Iterate(Write_Line'access);

               Close (File);

               Directory_Buffer.Append(New_Item => (To_Unbounded_String(Curr_Dir&"/"&To_String(FName)),
                                                    CharPad(Nick,15) & Subject) );

            else
               Display_Warning.Warning("Posting Cancelled");

            end if;

         end if;
      else
         Display_Warning.Warning("You Must be Logged In to Post");
      end if;

   end Post_Message;

end Message.Post;
