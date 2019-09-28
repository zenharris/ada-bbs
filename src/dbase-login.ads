with gnatcoll.SQL.Postgres;  use gnatcoll.SQL.Postgres;
with gnatcoll.SQL.Exec;      use gnatcoll.SQL.Exec;

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;


with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

with Texaco;
with Display_Warning;
with Dbase;
with Dbase.Scroller;

Package Dbase.Login is

   package SU renames Ada.Strings.Unbounded;
  -- package SUIO renames Ada.Text_IO.Unbounded_IO;

    procedure Create_User;

   procedure Login_User;


end Dbase.Login;
