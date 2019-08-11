with Message.Reader; use Message.Reader;




Package body Message.Login is

  procedure Read_Config (FileName : in String;
                          UserName : out Unbounded_String;
                          FullName : out Unbounded_String;
                          Password : out Unbounded_String) is

      HeaderType, HeaderText, scratch : Unbounded_String;
       File : File_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => Filename);

      scratch := SUIO.Get_Line(File);
      while scratch /= "" loop
         HeaderType := To_Unbounded_String(SU.Slice(scratch,1,SU.Index(scratch,":")-1));
         HeaderText := To_Unbounded_String(SU.Slice(scratch,SU.Index(scratch,":")+2,SU.Length(scratch)));
         if HeaderType = "UserName" then
            UserName := HeaderText;
         elsif HeaderType = "FullName" then
            FullName := HeaderText;
         elsif HeaderType = "Password" then
            Password := HeaderText;
         end if;
         scratch := SUIO.Get_Line(File);
      end loop;
      Close (File);
   exception
      when End_Error =>
         Close (File);
         null;
   end Read_Config;




 --  function FileExists (Namen : Unbounded_String) return Boolean is
 --     File : File_Type;
 --  begin
 --     Open (File => File,
 --           Mode => In_File,
 --           Name => To_String(Namen));
 --     Close (File);
 --     return True;
 --  exception
 --     when End_Error =>
 --        Close (File);
 --        return True;
 --     when ADA.IO_EXCEPTIONS.NAME_ERROR =>
 --        return False;
 --  end FileExists;

   procedure Create_User is
      File : File_Type;
      UserName, FullName, Password, Password2, Fname : Unbounded_String;
      NowDate : Time := Clock;
      Width,Columns : Column_Position := 50;
      Length,Lines : Line_Position := 8;
      Display_Window : Window;
   begin


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

         -- FName := UserName & ".cfg";

         Fname :=  To_Unbounded_String(Ada.Strings.Fixed.Translate(To_String(UserName),
                                       Ada.Strings.Maps.Constants.Lower_Case_Map));
         Fname := "users/" & FName & ".cfg";
         if Exists (To_String(FName)) then
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


         -- FName := UserName & ".cfg";
         Create (File => File,
                 Mode => Out_File,
                 Name => To_String(FName));

         SUIO.Put_Line(File,"UserName: " & UserName);
         SUIO.Put_Line(File,"FullName: " & FullName);
         SUIO.Put_Line(File,"Password: " & Password);
         SUIO.Put_Line(File,"CreateDate: " & To_Unbounded_String(Image (NowDate)));

         SUIO.Put_Line(File,To_Unbounded_String(""));


         Close (File);
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
      Fname := "users/" & Fname & ".cfg";

      if Exists (To_String(FName)) then
         Read_Config(To_String(Fname),UserName => UserName,
                     FullName => FullName,
                     Password => Password);
         if InputPassword = Password then

            UserLoggedIn := True;
            UserLoggedName := UserName;
            UserLoggedFullName := FullName;
            Display_Warning.Warning(To_String("Logged in "& UserLoggedName),Down => Integer(Length-1));

         else
            Display_Warning.Warning("Login Failed",Down => Integer(Length-1));
         end if;

      else
         Display_Warning.Warning("Login Failed",Down => Integer(Length-1));
      end if;


      Clear(Display_Window);
      Refresh(Display_Window);
      Delete (Win => Display_Window);

   end Login_User;




end Message.Login;
