with gnatcoll.SQL.Postgres;  use gnatcoll.SQL.Postgres;
with gnatcoll.SQL.Exec;      use gnatcoll.SQL.Exec;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Dbase is
  -- pragma Pure (Message);

   Version : constant String := "0.0.1";


   DB       : Database_Connection;

   UserLoggedIn : Boolean := False;
   UserLoggedName : Unbounded_String;
   UserLoggedFullName : Unbounded_String;
   UserLoggedUserId : Unbounded_String;

end Dbase;
