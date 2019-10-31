




package body Extools is

   protected Resource is
      entry Seize (Win : Window);
      procedure Release ;
   private
      Busy : Boolean := False;
   end Resource;

   protected body Resource is
      entry Seize (Win : Window) when not Busy is
      begin
         Busy := True;
         Refresh(Win);
         Busy := False;
      end Seize;

      procedure Release is
      begin
         Busy := False;
      end Release;
   end Resource;

   procedure Refrosh (Win : Window := Standard_Window) is
   begin
      Resource.Seize(Win);
   end Refrosh;


end Extools;
