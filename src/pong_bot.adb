


package body Pong_Bot is

   procedure Irc_Client is
    --  Bot : Irc.Bot.Connection; moved to pong_bot.ads
      c :Key_Code;
      Ch : Character;
      Edline : Unbounded_String;
      Lines : Line_Position;
      Columns : Column_Position;

      task Read_Loop is
         entry Start;
      end Read_Loop;

      task body Read_Loop is
         Col : Column_Position;
         Lin  : Line_Position;
       --  Line : Ada.Strings.Unbounded.Unbounded_String;
       --  Msg  : Irc.Message.Message;
      begin
         accept Start;
         loop
            declare
               Line : Ada.Strings.Unbounded.Unbounded_String;
               Msg  : Irc.Message.Message;

            begin
               Bot.Read_Line (Line);

               Msg := Irc.Message.Parse_Line (Line);
               Bot.Do_Message (Msg);

               --  Print out the message so we can get some feedback
               Get_Cursor_Position(Line => Lin,Column => Col);
               Msg.Print;
               Move_Cursor(Line => Lin,Column => Col);
               Refresh;
            exception
               when Irc.Message.Parse_Error =>     -- exit;
                  Irc.Message.Print_Line(To_Unbounded_String("Message Parse Error"));
            end;
         end loop;
      end Read_Loop;



   begin
      Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);
      Add (Column => 0,Line => Lines - 3,Str => "Enter Nick : ");
      Clear_To_End_Of_Line;
      Edline := To_Unbounded_String("");
      Texaco.Line_Editor(Standard_Window,
                         StartLine => Lines-3,
                         StartColumn => 14,
                         Editlength => 20,
                         Edline => Edline,
                         MaxLength => 20);
      Clear;
      Refresh;

      --  Specify the server, port, and nick of our bot
      Bot := Irc.Bot.Create ("irc.us.ircnet.net", 6667, Nick => To_String(Edline));

      --  Normally, you would use Irc.Commands.Install (Bot) to add
      --  the standard command set.
      Bot.On_Message ("PING", Irc.Commands.Ping_Server'Access);



      --  Connect the socket and identify the bot (send NICK and USER)
      Bot.Connect;
      Bot.Identify;

      Bot.Join ("#worldchat");

      --  Loop until program is killed or an error occurs
      Read_Loop.Start;

      loop
         Add (Standard_Window,
              Line => Lines-1,
              Column => 0,
              Str => " 1 Whois |2 Nick |3 Me |4 Version|5 Time |6 Clientinfo|7 Source| Esc exit");
         Refresh;

         Edline := To_Unbounded_String("");
         Texaco.Line_Editor(Standard_Window,
                            StartLine => Lines-2,
                            StartColumn => 0,
                            EditLength => Columns,
                            Edline => Edline,
                            MaxLength => Integer((Columns-1)*5)-2);
         c := Texaco.c;
         if c in Special_Key_Code'Range then
            case c is
            when Key_F1 =>
               Bot.Command(Cmd => "WHOIS",Args => To_String(Edline));


            when Key_F2 =>
               Bot.Command(Cmd => "NICK",Args => To_String(Edline));

               when Key_F3 =>

                  Bot.Privmsg ("#worldchat", Character'val(1)&"ACTION "& To_String(Edline) &Character'val(1));

               when Key_F4 =>

                  Bot.Privmsg (To_String(Edline), Character'val(1)&"VERSION"&Character'val(1));
               when Key_F5 =>

                  Bot.Privmsg (To_String(Edline), Character'val(1)&"TIME"&Character'val(1));
               when Key_F6 =>

                  Bot.Privmsg (To_String(Edline), Character'val(1)&"CLIENTINFO"&Character'val(1));
               when Key_F7 =>

                  Bot.Privmsg (To_String(Edline), Character'val(1)&"SOURCE"&Character'val(1));
               when others => null;

            end case;

         elsif c in Real_Key_Code'Range then

            Ch := Character'Val (c);
            case Ch is


            when CR | LF =>

               Bot.Privmsg ("#worldchat", To_String(Edline));
               Edline := To_Unbounded_String("");
               Texaco.Current_Char := 1;
            when ESC =>
               begin
                  Bot.Command("QUIT",":Exiting Normally");
                  exit;
               end;
            when others => null;

            end case;
         end if;
      end loop;


      Abort Read_Loop;

      --  Close the socket
      Bot.Disconnect;
   end Irc_Client;
end Pong_Bot;
