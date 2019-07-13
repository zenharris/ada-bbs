
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




--  This package contains the records of messages received from the
--  server in a convenient format.

with Ada.Strings.Unbounded;

package Irc.Message is

   package SU renames Ada.Strings.Unbounded;
   use type SU.Unbounded_String;

   --  Available automatically from Irc.Message.Message when received
   --  message is a PRIVMSG
   type Privmsg_Message is tagged record
      Target  : SU.Unbounded_String; --  nick/chan message was sent to
      Content : SU.Unbounded_String; --  content of the privmsg
   end record;

   --  Base message record, returned from Parse_Line, and passed
   --  around to handle commands, reply, etc.
   type Message is tagged record
      Sender : SU.Unbounded_String; --  When a command is sent from the
                                    --  server (PING, etc.) this will be
                                    --  an empty string.
      Command : SU.Unbounded_String;
      Args    : SU.Unbounded_String;

      Privmsg : Privmsg_Message;    --  Only exists when a PRIVMSG is sent
   end record;

   --  Given a line received from the server, parses and returns a
   --  Message record. Will raise Parse_Error if the message is in an
   --  unknown format.
   function Parse_Line (Line : in SU.Unbounded_String) return Message;

   --  Prints the message out to stdout in a readable format.
   --  Currently it is: Sender & "» " & Command & " " & Args
   procedure Print (This : Message);
   procedure Print_Line (Text_Line : SU.Unbounded_String);


   --  Raised by Parse_Line on bad lines.
   Parse_Error : exception;

private

   procedure Parse_Privmsg (Msg : in out Message);

end Irc.Message;
