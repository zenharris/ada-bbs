
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






package body Irc.Commands is

   procedure Install_Commands (Conn : in out Connection) is
   begin
      --  general commands
      Conn.On_Message ("001",      Join_On_Ident'Access);
      Conn.On_Message ("433",      Nick_In_Use'Access);
      Conn.On_Message ("PING",     Ping_Server'Access);
      Conn.On_Regexp  (".*",       Log_Line'Access);
   end Install_Commands;

   ----------------------------
   -- Begin general commands --
   ----------------------------

   procedure Join_On_Ident (Conn : in out Connection;
                            Msg  :        IrcMessage) is
      Channels : Bot.Unbounded_Vector.Vector
        := Conn.Get_Default_Channels;
   begin
      for I in Channels.First_Index .. Channels.Last_Index loop
         Conn.Join (SU.To_String (Channels.Element (I)));
      end loop;
   end Join_On_Ident;

   procedure Nick_In_Use (Conn : in out Connection;
                          Msg  :        IrcMessage) is
      use SU;

      Attr     : Bot.Nick_Attributes := Conn.Get_Attributes;
      New_Nick : SU.Unbounded_String := Attr.Nick & "_";
   begin
      Attr.Nick := New_Nick;
      Conn.Command (Cmd => "NICK", Args => SU.To_String (New_Nick));

      Conn.Set_Attributes (Attr);
   end Nick_In_Use;

   procedure Ping_Server (Conn : in out Connection;
                          Msg  :        Message.Message) is
   begin
      Conn.Command (Cmd => "PONG", Args => SU.To_String (Msg.Args));
   end Ping_Server;

   procedure Log_Line (Conn : in out Connection;
                       Msg  :        IrcMessage) is
   begin
      Msg.Print;
   end Log_Line;

end Irc.Commands;
