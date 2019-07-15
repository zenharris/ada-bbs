
-- This was an example bot now converted to an IRC Client
-- Copyright (c) 2019 Zen Harris

with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

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

      package FieldsVector is new Ada.Containers.Vectors (Natural,
                                                          Unbounded_String);
      use FieldsVector;
      Fields : FieldsVector.Vector;

      procedure Split (InVector : in out FieldsVector.Vector; InString : in Unbounded_String) is
         Cursor : Integer;
         scratch : Unbounded_String := InString;
         AppendStr : Unbounded_String;
      begin
         Cursor := Index(scratch," ");
         while  Cursor /= 0 loop
            AppendStr := To_Unbounded_String(Slice (Source => scratch,Low => 1,High => Cursor-1));
            if Length(AppendStr) > 0 then
               InVector.Append(AppendStr);
            end if;

            Delete(scratch,1,Cursor);
            Cursor := Index(scratch," ");
            if Cursor = 0 and then Length(scratch) > 0 then
               InVector.Append(scratch);
            end if;
         end loop;
      end Split;

      procedure Process_Command (CommandLine : Unbounded_String) is
      begin
         Clear(Fields);
         Split (Fields,CommandLine);

         if Fields.Length > 1 then

            if Fields.Element(0) = "/whois" then
               Bot.Command(Cmd => "WHOIS",Args => To_String(Fields.Element(1)));

            elsif Fields.Element(0) = "/nick" then
               Bot.Command(Cmd => "NICK",Args => To_String(Fields.Element(1)));

            elsif Fields.Element(0) = "/me" then
               Bot.Privmsg ("#worldchat", Character'val(1)&"ACTION "&
                              To_String(Unbounded_Slice(CommandLine,index(CommandLine," ")+1,Length(CommandLine)) )
                            &Character'val(1));

            elsif Fields.Element(0) = "/version" then
               Bot.Privmsg (To_String(Fields.Element(1)),
                            Character'val(1)&"VERSION"&Character'val(1));

            elsif Fields.Element(0) = "/time" then
               Bot.Privmsg (To_String(Fields.Element(1)), Character'val(1)&"TIME"&Character'val(1));

            elsif Fields.Element(0) = "/clientinfo" then
               Bot.Privmsg (To_String(Fields.Element(1)), Character'val(1)&"CLIENTINFO"&Character'val(1));

            elsif Fields.Element(0) = "/source" then
               Bot.Privmsg (To_String(Fields.Element(1)), Character'val(1)&"SOURCE"&Character'val(1));

            end if;
         else
            if Fields.Element(0) = "/help" then
               Irc.Message.Print_Line(To_Unbounded_String("/whois <nickname>           /nick <nickname> "));
               Irc.Message.Print_Line(To_Unbounded_String("/me <action description>    /version <nickname> "));
               Irc.Message.Print_Line(To_Unbounded_String("/time <nickname>            /clientinfo <nickname> "));
               Irc.Message.Print_Line(To_Unbounded_String("/source <nickname>          /quit "));
            end if;
         end if;

      end Process_Command;


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


      c := Texaco.c;
      if Character'Val (c) /= ESC then

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
                 Str => "/help for commands  Esc exit");
            Refresh;

            Edline := To_Unbounded_String("");
            Texaco.Line_Editor(Standard_Window,
                               StartLine => Lines-2,
                               StartColumn => 0,
                               EditLength => Columns,
                               Edline => Edline,
                               MaxLength => 400);
            c := Texaco.c;
            if c in Special_Key_Code'Range then
               case c is
               when Key_F1 => null;

               when others => null;

               end case;

            elsif c in Real_Key_Code'Range then

               Ch := Character'Val (c);
               case Ch is

               when CR | LF =>

                  if Index(Source => Edline,Pattern => "/") = 1 then
                     Process_Command(Edline);
                  else
                     Bot.Privmsg ("#worldchat", To_String(Edline));
                  end if;

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


         -- Abort Read_Loop;

         --  Close the socket
         Bot.Disconnect;
      end if;
      Abort Read_Loop;

   end Irc_Client;
end Pong_Bot;
