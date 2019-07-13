with Irc.Bot;
with Irc.Commands;
with Irc.Message;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Texaco;

with GNAT.Sockets;


package Pong_Bot is

    Bot : Irc.Bot.Connection;

   procedure Irc_Client;
end Pong_Bot;
