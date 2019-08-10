
-- This was an example bot now converted to an IRC Client
-- Copyright (c) 2019 House Harris Software - Zen Harris

with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

With Irc.Commands; use Irc.Commands;
with Irc.Message; use Irc.Message;
with Irc.Bot; use Irc.Bot;

with GNAT.Regpat; use GNAT.Regpat;

with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

package body Pong_Bot is
   package SU renames Ada.Strings.Unbounded;

   procedure Irc_Client is
    --  Bot : Irc.Bot.Connection; moved to pong_bot.ads
      c :Key_Code;
      Ch : Character;
      Edline : Unbounded_String;
      Channel : Unbounded_String := To_Unbounded_String("#worldchat");
      Lines : Line_Position;
      Columns : Column_Position;
      Quit : Boolean := False;

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

      procedure Clear_Response;

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

         if Cursor /= 0 then
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
         else
            if Length(scratch) > 0 then
               InVector.Append(scratch);
            end if;

         end if;

      end Split;

      function FieldsMerge (MergeStart : integer; MergeEnd : Integer ) return Unbounded_String is
         OutString : Unbounded_String := To_Unbounded_String("");
      begin
         for i in MergeStart..MergeEnd loop
            Append(OutString,Fields.Element(i) & " ");
         end loop;
         return OutString;
      end FieldsMerge;


      Suppress_Response : Boolean := True;
      Local_Response : Boolean := True;


      procedure Process_Command (CommandLine : Unbounded_String; Quit : in out Boolean ) is
      begin
         Clear(Fields);
         Split (Fields,CommandLine);

         if Fields.Length > 1 then

            if Fields.Element(0) = "/whois" then
               Bot.Command(Cmd => "WHOIS",Args => To_String(Fields.Element(1)));

            elsif Fields.Element(0) = "/nick" then
               Bot.Command(Cmd => "NICK",Args => To_String(Fields.Element(1)));

            elsif Fields.Element(0) = "/me" then
               Bot.Privmsg (To_String(Channel), Character'val(1)&"ACTION "&
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

            elsif Fields.Element(0) = "/response" then

               if Fields.Element(1) = "on" then
                  --Bot.On_Regexp(OnRegexp => ".*",Func => Response_Processor'Unrestricted_Access);
                  Suppress_Response := False;
                  Local_Response := False;
                  Irc.Message.Print_Line(To_Unbounded_String("Response Processor On"));
               elsif Fields.Element(1) = "off" then
                  Suppress_Response := True;
                  Irc.Message.Print_Line(To_Unbounded_String("Response Processor Off"));
               elsif Fields.Element(1) = "local" then
                  Suppress_Response := False;
                  Local_Response := True;
                  Irc.Message.Print_Line(To_Unbounded_String("Response Processor On Local"));
               elsif Fields.Element(1) = "clear" then
                  Clear_Response;
                  Irc.Message.Print_Line(To_Unbounded_String("Response Processor Cleared"));
               end if;

            elsif Fields.Element(0) = "/msg" then
               if Fields.Length > 2 then

                  Bot.Privmsg (To_String(Fields.Element(1)),
                               --To_String(Unbounded_Slice(CommandLine,index(CommandLine," ")+1,Length(CommandLine)) )
                               To_String(FieldsMerge(2,Integer(Fields.Length)-1))
                              );
               end if;

            end if;
         else
            if Fields.Length = 1 then

               if Fields.Element(0) = "/help" then
                  Irc.Message.Print_Line(To_Unbounded_String("/whois <nickname>              /nick <nickname> "));
                  Irc.Message.Print_Line(To_Unbounded_String("/me <action description>       /version <nickname> "));
                  Irc.Message.Print_Line(To_Unbounded_String("/time <nickname>               /clientinfo <nickname> "));
                  Irc.Message.Print_Line(To_Unbounded_String("/source <nickname>             /msg <nickname> "));
                  Irc.Message.Print_Line(To_Unbounded_String("/response <on|off|local|clear> /quit "));
               elsif Fields.Element(0) = "/quit" then
                  Quit := True;

               end if;
            end if;

         end if;

      end Process_Command;




      type String_Access is access String;

      type Response_Array is Array (0 .. 2) of String_Access;

      type Response_Record is record
         MatchRegex : String_Access;
         Response : Response_Array;
         Counter : Integer;
      end record;
      type Response_Type is array (Positive range <>) of Response_Record;

      ResponseTable : Response_Type  :=
        ((new String'(".*\bsun\b.*"),
         (new String'("The Sun's coming up, like a big bald head"),new String'("Sun and Moon"),new String'("That'a alot of sun")),0),
         (new String'(".*\b[Hh]ypnotoad\b.*|.*\b[Ff]uturama\b.*|.*\bpraise\b.*"),
          (new String'("All Praise to The Hypnotoad GRGGRGRRBRBBRBRRGRGGRGRRBRBBRBRR"),
           new String'("GRGGRGRRBRBBRBRRGR"),
         new String'("All Praise, All Praise")),0),
         (new String'(".*\bconjob\b.*|.*\bconm[ea]n\b.*|.*\bbusted\b.*|.*\billegal\b.*"),
          (new String'("Conjob Right on"),new String'("You;ve been ripped off man"),new String'("You're saying conjob alot")),0),
         (new String'(".*\bwine\b.*|.*\bdrink\b.*"),
          (new String'("Try the wine!"),new String'("I see you enjoya fine wine, Have another Glass!"),new String'("And the water turned wine")),0),
         (new String'(".*\bcat\b.*"),
          (new String'("Hep Cats"),new String'("Cool for Cats"),new String'("Stop Saying the word cat")),0),
         (new String'(".*\bfish\b.*"),
          (new String'("Neither fish nor flesh"),new String'("and to eat no fish"),new String'("But enough of fish")),0),
         (new String'(".*\bcoffee\b.*"),
          (new String'("I feel disoriented if I can't go to the cafe in the morning"),
           new String'("Coffee rich Coffee strong"),
         new String'("Beans of madness")),0),
         (new String'(".*\bweather\b.*"),
          (new String'("The weather's on the change"),
           new String'("Lines form in feint dischord"),new String'("And the stormwatch brews a concert of kings")),0));

      procedure Clear_Response is
      begin
         for i in ResponseTable'Range loop
            ResponseTable(i).Counter := 0;
         end loop;
      end Clear_Response;


      procedure Response_Processor (Conn : in out Irc.Commands.Connection;
                                    Msg  :        IrcMessage) is

         Regex : GNAT.Regpat.Pattern_Matcher (1024);
         Matches : GNAT.Regpat.Match_Array (0 .. 1);
         Now : Time;
         Next : Time;
         D    : Duration := 0.05;
         L  : Integer;
         scratch : Unbounded_String;
      begin
         if Suppress_Response = False then
            for i in ResponseTable'Range loop
               GNAT.Regpat.Compile (Regex, ResponseTable(i).MatchRegex.all);

               scratch :=  SU.Unbounded_Slice(Msg.Args,SU.Index(Msg.Args,":")+1,SU.Length( Msg.Args));

               scratch := To_Unbounded_String(Ada.Strings.Fixed.Translate(To_String(scratch),
                                              Ada.Strings.Maps.Constants.Lower_Case_Map));


               GNAT.Regpat.Match (Regex, To_String(scratch) , Matches);

               if Matches (0) /= GNAT.Regpat.No_Match then
                  --  Pair.Func (This, Msg);


                  if ResponseTable(i).Counter <= ResponseTable(i).Response'Last then
                     Now := Clock;
                     L := ResponseTable(i).Response(ResponseTable(i).Counter).all'Length;
                     Next := Now + (D*L) ;
                     delay until Next;

                     if Local_Response then
                        Irc.Message.Print_Line(To_Unbounded_String(
                                               ResponseTable(i).Response(ResponseTable(i).Counter).all));
                     else
                        Bot.Privmsg (To_String(Channel),
                                     ResponseTable(i).Response(ResponseTable(i).Counter).all);
                     end if;
                     ResponseTable(i).Counter := ResponseTable(i).Counter + 1;
                  end if;
               end if;

            end loop;
         end if;

      end Response_Processor;



   begin
      Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);
      Add (Column => 0,Line => Lines - 3,Str => "Enter Nick : ");
      Clear_To_End_Of_Line;

      Edline := To_Unbounded_String("");
      Texaco.Line_Editor(Standard_Window,
                         StartLine => Lines-3,
                         StartColumn => 14,
                         Editlength => 21,
                         Edline => Edline,
                         MaxLength => 20,
                        SuppressSpaces => True);


      c := Texaco.c;
      if Character'Val (c) /= ESC then

         Clear;
         Refresh;

         --  Specify the server, port, and nick of our bot
         Bot := Irc.Bot.Create ("irc.us.ircnet.net", 6667, Nick => To_String(Edline));

         --  Normally, you would use Irc.Commands.Install (Bot) to add
         --  the standard command set.

         Bot.On_Message ("PING", Irc.Commands.Ping_Server'Access);

         -- Bot.On_Regexp(OnRegexp => ".*",Func => Response_Processor'Unrestricted_Access);
         Bot.On_Regexp(OnRegexp => "^PRIVMSG",Func => Response_Processor'Unrestricted_Access);


         --  Connect the socket and identify the bot (send NICK and USER)
         Bot.Connect;
         Bot.Identify;

         Bot.Join (To_String(Channel));

         --  Loop until program is killed or an error occurs
         Read_Loop.Start;

         loop
            Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);
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
                     Process_Command(Edline,Quit);
                     if Quit then
                        Bot.Command("QUIT",":Exiting Normally");
                        exit;
                     end if;
                  else
                     Bot.Privmsg (To_String(Channel), To_String(Edline));
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

         --  Close the socket
         Bot.Disconnect;
      end if;

      Abort Read_Loop;

   end Irc_Client;
end Pong_Bot;
