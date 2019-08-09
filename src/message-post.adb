with Message.Reader; use Message.Reader;

package body Message.Post is

  procedure Post_Message (ReplyID : in Unbounded_String := To_Unbounded_String("");
                          ReplySubject : in Unbounded_String := To_Unbounded_String("")) is
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
                            Editlength => 15,
                            Edline => Nick,
                            MaxLength => 15);
         exit when Nick /= "";
      end loop;

      Add (Line => 2,Column => 0,Str => "Subject : ");
      if (SU.Length(ReplySubject) > 0) then

         if SU.Index(ReplySubject,"Re.") = 1 then
            Subject := ReplySubject;
         else
            Subject := "Re. " & ReplySubject;
         end if;

      end if;

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
            if SU.Length(ReplyID) /= 0 then
               SUIO.Put_Line(File,"ReplyTo: " & ReplyID);
            end if;

            SUIO.Put_Line(File,To_Unbounded_String(""));

            Text_Buffer.Iterate(Write_Line'access);

            Close (File);

            Directory_Buffer.Append(New_Item => (To_Unbounded_String(Curr_Dir&"/messages/"&To_String(FName)),
                                                 CharPad(Nick,15) & Subject) );

         else
            Add (Line => 3,Column => 0, Str => "Cancelling Message");
            Clear_To_End_Of_Line;
            Refresh;
         end if;

      end if;

   end Post_Message;

end Message.Post;
