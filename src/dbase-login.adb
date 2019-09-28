


Package body Dbase.Login is
   CIB : Direct_Cursor;

   function If_Exists (Usrnme : Unbounded_String) return Boolean is
      Stmt : Prepared_Statement;

      SQLstatement : Unbounded_String;
   begin

      SQLstatement := "SELECT * FROM users WHERE username = '"& Usrnme & "'";

         Add (Standard_Window,
              Line => 1,
              Column => 1,
           Str => To_String(SQLstatement));
      refresh;

      Stmt:= Prepare (To_String(SQLstatement), Index_By => Field_Index'First);

      CIB.Fetch (Dbase.DB, Stmt);

      if CIB.Has_Row then
         return True;
      else
         return False;
      end if;



   end If_Exists;

   function Fld (CI : Direct_Cursor; FldNme : String) return Unbounded_String is
   begin
      for i in 0..CI.Field_Count-1 loop
         if CI.Field_Name(i) = FldNme then
            return To_Unbounded_String(CI.Value(i));
         end if;
      end loop;
      Display_Warning.Warning("No Field " & FldNme);
      return To_Unbounded_String(FldNme) & " Not Found";
   end;


   procedure Create_User is

      UserName, FullName, Password, Password2, Fname : Unbounded_String;
      NowDate : Time := Clock;
      CreateDate, SQLstatement : Unbounded_String;
      Width,Columns : Column_Position := 50;
      Length,Lines : Line_Position := 8;
      Display_Window : Window;

      Stmt : Prepared_Statement;
   begin

      if Dbase.Scroller.OpenDb then


         Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);

         Display_Window := Sub_Window(Win => Standard_Window,
                                      Number_Of_Lines => Length,
                                      Number_Of_Columns => Width,
                                      First_Line_Position => (Lines - Length) / 2,
                                      First_Column_Position => (Columns - Width) / 2);

         Clear(Display_Window);
         Box(Display_Window);



         loop
            Add (Display_Window,Line => 2,Column => 2,Str => "UserName : ");
            Texaco.Line_Editor(Display_Window,
                               StartLine => 2,
                               StartColumn => 15,
                               Editlength => 16,
                               Edline => UserName,
                               MaxLength => 15,
                               SuppressSpaces => True);


            Fname :=  To_Unbounded_String(Ada.Strings.Fixed.Translate(To_String(UserName),
                                          Ada.Strings.Maps.Constants.Lower_Case_Map));

            if If_Exists (FName) then
               Display_Warning.Warning("That Username is already in use",Down => Integer(Length-1));
               UserName := To_Unbounded_String("");
               Redraw(Display_Window,0,Integer(Length)-1);
               Refresh(Display_Window);
            end if;


            exit when UserName /= "";
         end loop;


         loop
            Add (Display_Window,Line => 3,Column => 2,Str => "Full Name : ");
            Texaco.Line_Editor(Display_Window,
                               StartLine => 3,
                               StartColumn => 15,
                               Editlength => 31,
                               Edline => FullName,
                               MaxLength => 30);
            exit when FullName /= "";
         end loop;


         loop
            Add (Display_Window,Line => 4,Column => 2,Str => "Password : ");
            Texaco.Password_Editor(Display_Window,
                                   StartLine => 4,
                                   StartColumn => 15,
                                   Edline => Password,
                                   MaxLength => 15);
            Add (Display_Window,Line => 5,Column => 2,Str => "Re-Type Password : ");
            Texaco.Password_Editor(Display_Window,
                                   StartLine => 5,
                                   StartColumn => 21,
                                   Edline => Password2,
                                   MaxLength => 15);

            if Password /= Password2 then
               Display_Warning.Warning("Passwords Do Not Match",Down => Integer(Length-1));
               Password := To_Unbounded_String("");
               Password2 := To_Unbounded_String("");

            end if;


            exit when Password /= "";
         end loop;

         if Display_Warning.GetYN("Do You want to save this User Y/N",Down => Integer(Length-1)) then

            CreateDate := To_Unbounded_String(Image (NowDate));

            SQLstatement := SQLstatement & "INSERT INTO users (username,fullname,password) VALUES (";
            SQLstatement := SQLstatement & "'"&Fname&"','"&FullName&"','"&Password&"')";
            Add (Standard_Window,
                 Line => 1,
                 Column => 1,
                 Str => To_String(SQLstatement));
            refresh;

            Stmt:= Prepare (To_String(SQLstatement));


            Dbase.DB.Execute(Stmt);
            Dbase.DB.Commit;

            if Dbase.DB.Success then

               Display_Warning.Warning("New User Created");
            else
               Display_Warning.Warning("Create User Failed");
            end if;




         end if;
         Dbase.Scroller.CloseDb;
      else
         Display_Warning.Warning("No Open Universe DB");
      end if;

      Clear(Display_Window);
      Refresh(Display_Window);
      Delete (Win => Display_Window);

   end Create_User;

   procedure Login_User is
      InputUserName, InputPassword, Fname, UserName,FullName,Password : Unbounded_String;
      Width,Columns : Column_Position := 50;
      Length,Lines : Line_Position := 8;
      Display_Window : Window;
   begin

      if Dbase.Scroller.OpenDb then



         Get_Size(Number_Of_Lines => Lines,Number_Of_Columns => Columns);

         Display_Window := Sub_Window(Win => Standard_Window,
                                      Number_Of_Lines => Length,
                                      Number_Of_Columns => Width,
                                      First_Line_Position => (Lines - Length) / 2,
                                      First_Column_Position => (Columns - Width) / 2);

         Clear(Display_Window);
         Box(Display_Window);



         loop
            Add (Display_Window,Line => 2,Column => 2,Str => "UserName : ");
            Texaco.Line_Editor(Display_Window,
                               StartLine => 2,
                               StartColumn => 15,
                               Editlength => 16,
                               Edline => InputUserName,
                               MaxLength => 15,
                               SuppressSpaces => True);

            exit when InputUserName /= "";
         end loop;


         loop
            Add (Display_Window,Line => 3,Column => 2,Str => "Password : ");
            Texaco.Password_Editor(Display_Window,
                                   StartLine => 3,
                                   StartColumn => 15,
                                   Edline => InputPassword,
                                   MaxLength => 15);
            exit when InputPassword /= "";
         end loop;


         Fname :=  To_Unbounded_String(Ada.Strings.Fixed.Translate(To_String(InputUserName),
                                       Ada.Strings.Maps.Constants.Lower_Case_Map));


         if IF_Exists (FName) then


            Password := Fld(CIB,"password");

            if InputPassword = Password then

               UserLoggedIn := True;
               UserLoggedName := Fld(CIB,"username");
               UserLoggedFullName := Fld(CIB,"fullname");
               UserLoggedUserId := Fld(CIB,"user_id");

               Display_Warning.Warning(To_String("Logged in "& UserLoggedName),Down => Integer(Length-1));

            else
               Display_Warning.Warning("Login Failed",Down => Integer(Length-1));
            end if;

         else
            Display_Warning.Warning("Login Failed",Down => Integer(Length-1));
         end if;
         Dbase.Scroller.CloseDb;
      end if;


      Clear(Display_Window);
      Refresh(Display_Window);
      Delete (Win => Display_Window);

   end Login_User;




end Dbase.Login;
