with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
With Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO;
--with Ada.Directories;
use Ada.Text_IO;
--use Ada.Directories;

with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

with Texaco;
with Display_Warning;
with Ada.IO_Exceptions;


Package Message.Login is

   package SU renames Ada.Strings.Unbounded;
   package SUIO renames Ada.Text_IO.Unbounded_IO;

    procedure Create_User;

   procedure Login_User;


end Message.Login;
