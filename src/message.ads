with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Message is
  -- pragma Pure (Message);

   Version : constant String := "0.0.1";

   UserLoggedIn : Boolean := False;
   UserLoggedName : Unbounded_String;
   UserLoggedFullName : Unbounded_String;

end Message;
