
-- (The MIT License)

--Copyright (c) 2012 Erik Price

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights to
-- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is furnished to
-- do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
-- INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
-- PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
-- OR OTHER DEALINGS IN THE SOFTWARE.





-- Print and Print_Line severly modified for Ncurses
-- And use as an IRC Client
-- MIT License
-- Copyright (c) 2019 Zen Harris



with Ada.Text_IO;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Display_Warning;
with Pong_Bot;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package body Irc.Message is

   function Parse_Line (Line : in SU.Unbounded_String) return Message is
      Msg    : Message;
      Index  : Natural  := 2;
      Start, Finish : Natural := 0;
      Size   : Natural := SU.Length (Line);

      procedure Read_Word;
      procedure Read_Word is
         Next_WS : Natural := SU.Index (Line, " ", Index);
      begin
         Start := Index;

         if Next_WS > Size then
            raise Parse_Error;
         end if;

         Finish := Next_WS - 1;
         Index := Next_WS + 1;
      end Read_Word;

   begin

      if SU.To_String (Line) (1) /= ':' then

         if SU.Index (Line, "PING") = 1 then
            Msg.Sender := SU.To_Unbounded_String ("");
            Msg.Command := SU.To_Unbounded_String ("PING");
            Msg.Args := SU.Unbounded_Slice (Line, 1 + 6, Size);

            return Msg;
         end if;

         raise Parse_Error;
      end if;

      Read_Word;
      Msg.Sender  := SU.Unbounded_Slice (Line, Start, Finish);

      Read_Word;
      Msg.Command := SU.Unbounded_Slice (Line, Start, Finish);

      Msg.Args    := SU.Unbounded_Slice (Line, Finish + 2, Size);

      if Msg.Command = "PRIVMSG" then
         Msg.Parse_Privmsg;
      end if;

      return Msg;
   end Parse_Line;


   -- Print and Print_Line severly modified for Ncurses
   -- And use as an IRC Client
   -- MIT License
   -- Copyright (c) 2019 Zen Harris


   Current_Line : Line_Position := 3;

   procedure Print_Line (Text_Line : SU.Unbounded_String) is
      printstr : SU.Unbounded_String;
      Lines : Line_Position;
      Columns : Column_Position;
      Curs : Integer := 1;
      ScrollRegionTop, ScrollRegionBottom : Line_Position;

   begin
      Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);

      ScrollRegionTop := 3;
      ScrollRegionBottom := Lines - 3;


      if (Current_Line < ScrollRegionBottom) then
         Current_Line := Current_Line + 1;
      else

         Move_Cursor (Line => ScrollRegionTop, Column => 0);
         Delete_Line;
         Move_Cursor (Line => ScrollRegionBottom, Column => 0);
         Insert_Line;
      end if;


      if SU.Length(Text_Line) > Integer(Columns) then
         while Curs < SU.Length(Text_Line) loop
            if Curs + Integer(Columns) > SU.Length(Text_Line) then
               printstr := SU.Unbounded_Slice(Text_Line,Curs,SU.Length(Text_Line));
            else
               printstr := SU.Unbounded_Slice(Text_Line,Curs,Curs+Integer(Columns));
            end if;

            Add(Win => Standard_Window,
                Column => 0,
                Line => Current_Line-1,
                Str => Su.To_String(printstr) );
            Curs := Curs + Integer(Columns);

            if (Current_Line < ScrollRegionBottom) then
               Current_Line := Current_Line + 1;
            else

               Move_Cursor (Line => ScrollRegionTop, Column => 0);
               Delete_Line;
               Move_Cursor (Line => ScrollRegionBottom, Column => 0);
               Insert_Line;

            end if;
            Refresh;

         end loop;

         --   printstr := SU.Unbounded_Slice(Text_Line,Curs-Integer(Columns),SU.Length(Text_Line));
      else
         --   printstr := Text_Line;


         Add(Win => Standard_Window,
             Column => 0,
             Line => Current_Line-1,
             Str => Su.To_String(Text_Line) );

      end if;

      Refresh;
   end;

   -- Print and Print_Line severly modified for Ncurses
   -- and use as an IRC Client
   -- MIT License
   -- Copyright (c) 2019 Zen Harris


   ClientVersion : constant String := "0.1.10 beta";

   procedure Print (This : Message) is
      use Ada.Text_IO;
      SenderNick,SenderArgs,MsgTarget : SU.Unbounded_String;

      VersionReply   : SU.Unbounded_String :=
        SU.To_Unbounded_String(Character'Val(01)&"VERSION Ada IRC Ver "& ClientVersion &" Running on Linux"&Character'Val(01));
      ClientInfoReply   : SU.Unbounded_String :=
        SU.To_Unbounded_String(Character'Val(01)&"CLIENTINFO VERSION TIME SOURCE CLIENTINFO ACTION"&Character'Val(01));

      SourceReply   : SU.Unbounded_String :=
        SU.To_Unbounded_String(Character'Val(01)&"SOURCE https://github.com/zenharris/ada-bbs"&Character'Val(01));

      Now : Time;

      package FieldsVector is new Ada.Containers.Vectors (Natural,
                                                          SU.Unbounded_String);
      use FieldsVector;
      Fields : FieldsVector.Vector;

      procedure Split (InVector : in out FieldsVector.Vector; InString : in SU.Unbounded_String) is
         Cursor : Integer;
         scratch : SU.Unbounded_String := InString;
         AppendStr : SU.Unbounded_String;
      begin
         Cursor := SU.Index(scratch," ");
         while  Cursor /= 0 loop
            AppendStr := To_Unbounded_String(Slice (Source => scratch,Low => 1,High => Cursor-1));
            if Length(AppendStr) > 0 then
               InVector.Append(AppendStr);
            end if;
           -- InVector.Append(SU.To_Unbounded_String(SU.Slice (Source => scratch,Low => 1,High => Cursor-1)));
            SU.Delete(scratch,1,Cursor);
            Cursor := SU.Index(scratch," ");
            if Cursor = 0 and then SU.Length(scratch) > 0 then
               InVector.Append(scratch);
            end if;
         end loop;
      end Split;

   begin

      if This.Command = "PRIVMSG" then
         SenderArgs := SU.Unbounded_Slice(This.Args,SU.Index(This.Args,":")+1,SU.Length( This.Args));
         MsgTarget := SU.Unbounded_Slice(This.Args,1,SU.Index(This.Args,":")-1);
         SenderNick := SU.Unbounded_Slice(This.Sender,1,SU.Index(This.Sender,"!")-1);

        -- if SU.Index(SenderArgs,Character'Val(1)&"VERSION"&Character'Val(1)) = 1 then
         if SU.Index( SenderArgs,Character'Val(01)&"VERSION"&Character'Val(01)) = 1 then

            Pong_Bot.Bot.Command(Cmd => "NOTICE",
                                 Args => SU.To_String(SenderNick) &" :"&
                                   SU.To_String(VersionReply));

         elsif SU.Index( SenderArgs,Character'Val(01)&"TIME"&Character'Val(01)) = 1 then

            Now := Clock;
            Pong_Bot.Bot.Command(Cmd => "NOTICE",
                                 Args => SU.To_String(SenderNick) &" :"&
                                   Character'Val(01)&"TIME "&Image (Now)&Character'Val(01));

         elsif SU.Index( SenderArgs,Character'Val(01)&"CLIENTINFO"&Character'Val(01)) = 1 then

            Pong_Bot.Bot.Command(Cmd => "NOTICE",
                                 Args => SU.To_String(SenderNick) &" :"&
                                   SU.To_String(ClientInfoReply));

         elsif SU.Index( SenderArgs,Character'Val(01)&"SOURCE"&Character'Val(01)) = 1 then

            Pong_Bot.Bot.Command(Cmd => "NOTICE",
                                 Args => SU.To_String(SenderNick) &" :"&
                                   SU.To_String(SourceReply));

         elsif SU.Index(SenderArgs,Character'Val(01)&"ACTION") = 1 then
            Print_Line ( "* " & SenderNick &
                 SU.Unbounded_Slice(SenderArgs,SU.Index(SenderArgs," "),SU.Length( SenderArgs)-1));
         else
            if Su.Index(MsgTarget,"#") = 1 then
               Print_Line ( "<" & SenderNick & "> " & SenderArgs);
            else
               Print_Line ( "*" & SenderNick & "* " & SenderArgs);
            end if;
         end if;
      elsif This.Command = "NOTICE" then

         SenderNick := SU.Unbounded_Slice(This.Sender,1,SU.Index(This.Sender,"!")-1);

         SenderArgs := SU.Unbounded_Slice(This.Args,SU.Index(This.Args,":")+1,SU.Length( This.Args));
         -- strip off header /00 and tail /00

         if SU.Index(SenderArgs,Character'Val(01)&"TIME") = 1 then

            SenderArgs := SU.Unbounded_Slice(SenderArgs,Index(SenderArgs," "),SU.Length(SenderArgs)-1);
            Print_Line ("*** Time Reply from "& SenderNick &
                          " is " & SenderArgs );

         elsif SU.Index(SenderArgs,Character'Val(01)&"VERSION") = 1 then

            SenderArgs := SU.Unbounded_Slice(SenderArgs,Index(SenderArgs," "),SU.Length(SenderArgs)-1);
            Print_Line ("*** Version Reply "& SenderNick &
                          " is running " & SenderArgs );

         elsif SU.Index(SenderArgs,Character'Val(01)&"CLIENTINFO") = 1 then

            SenderArgs := SU.Unbounded_Slice(SenderArgs,Index(SenderArgs," "),SU.Length(SenderArgs)-1);
            Print_Line ("*** ClientInfo "& SenderNick &
                          " supports " & SenderArgs );

         elsif SU.Index(SenderArgs,Character'Val(01)&"SOURCE") = 1 then

            SenderArgs := SU.Unbounded_Slice(SenderArgs,Index(SenderArgs," "),SU.Length(SenderArgs)-1);
            Print_Line ("*** Source Reply "& SenderNick &
                          " available at " & SenderArgs );

         end if;

      elsif This.Command = "QUIT" then
         Print_Line ("*** Signoff: " & SU.Unbounded_Slice(This.Sender,1,SU.Index(This.Sender,"!")-1) &
                       " " & SU.Unbounded_Slice(This.Args,SU.Index(This.Args,":")+1,SU.Length( This.Args)));
      elsif This.Command = "PART" then
         Print_Line ("*** "& SU.Unbounded_Slice(This.Sender,1,SU.Index(This.Sender,"!")-1) &
                       " has left channel(part) "& SU.Unbounded_Slice(This.Args,1,SU.Index(This.Args,":")-1) &
                       "("& SU.Unbounded_Slice(This.Args,SU.Index(This.Args,":")+1,SU.Length( This.Args))&")");
      elsif This.Command = "NICK" then
         Print_Line ("*** "& SU.Unbounded_Slice(This.Sender,1,SU.Index(This.Sender,"!")-1) &
                       " is now known as " & SU.Unbounded_Slice(This.Args,SU.Index(This.Args,":")+1,SU.Length( This.Args)));
      elsif This.Command = "JOIN" then
         Print_Line ("*** " & SU.Unbounded_Slice(This.Sender,1,SU.Index(This.Sender,"!")-1) &
                       " (" & SU.Unbounded_Slice(This.Sender,SU.Index(This.Sender,"!")+1,SU.Length(This.Sender)) & ") " &
                       "has joined " & SU.Unbounded_Slice(This.Args,SU.Index(This.Args,":")+1,SU.Length( This.Args)));
      elsif This.Command = "MODE" then
         Print_Line ("*** Mode change " & SU.Unbounded_Slice(This.Args,SU.Index(This.Args," ")+1,SU.Length( This.Args)) &
                       " by " & This.Sender );

      elsif This.Command = "311" then

         Split(Fields,This.Args);

         Print_Line ("*** " & Fields.Element(1) & " is " &
                       Fields.Element(2) &"@"& Fields.Element(3) &
                       "("&
                       SU.Unbounded_Slice(This.Args,SU.Index(This.Args,":")+1,SU.Length(This.Args))
                     & ")");

         Clear(Fields);

      elsif This.Command = "312" then

         Split(Fields,This.Args);

         Print_Line ("*** on irc via server " & Fields.Element(2) &
                       "("&
                       SU.Slice(This.Args,Su.Index(This.Args,":")+1,SU.Length(This.Args))
                     &")");

         Clear(Fields);

      elsif This.Command = "319" then

         Print_Line ("*** on channels: " & SU.Unbounded_Slice(Source => This.Args,
                                                    Low => SU.Index(This.Args,":")+1,
                                                    High => SU.Length(This.Args)));
       elsif This.Command = "318" then

         Print_Line ("*** " & SU.Unbounded_Slice(Source => This.Args,
                                                    Low => SU.Index(This.Args,":")+1,
                                                    High => SU.Length(This.Args)));
       elsif This.Command = "317" then

         Split(Fields,This.Args);

         Print_Line ("*** " & Fields.Element(2) &" "&
                       Fields.Element(3) &" "&
                       SU.Unbounded_Slice(This.Args,SU.Index(This.Args,":")+1,SU.Length(This.Args)));

         Clear(Fields);

      elsif This.Command = "PING" then null;

      else

         Print_Line(This.Sender & " |" & This.Command & "|" &  This.Args);

      end if;
      Refresh;


     -- Ada.Text_IO.Put_Line
     --   (SU.To_String
     --      (This.Sender & "» " & This.Command & " " & This.Args));
   end Print;

   -- End of modifications for Ncurses
   -- and use as an IRC Client
   -- MIT License
   -- Copyright (c) 2019 Zen Harris

   procedure Parse_Privmsg (Msg : in out Message) is
   begin

      Msg.Privmsg.Target  := SU.Unbounded_Slice
        (Msg.Args, 1, SU.Index (Msg.Args, " ") - 1);

      Msg.Privmsg.Content := SU.Unbounded_Slice
        (Msg.Args, SU.Index (Msg.Args, ":"), SU.Length (Msg.Args));

      --  message sent to nick directly instead of in a channel
      if SU.To_String (Msg.Privmsg.Target) (1) /= '#' then
         Msg.Privmsg.Target := SU.To_Unbounded_String
           (SU.Slice (Msg.Sender, 1, SU.Index (Msg.Sender, "!") - 1));
      end if;

   end Parse_Privmsg;

end Irc.Message;
