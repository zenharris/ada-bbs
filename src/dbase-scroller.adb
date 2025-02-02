with Display_Warning;
with Dbase.Login;

with Templates;
-- with Dbase.DrackSpace;
with Extools; use Extools;
with Process_Menu;-- use Process_Menu;


package body Dbase.Scroller is
   DB_Descr : Database_Description;
   -- DB       : Database_Connection;



 -- in .ads  Definition_Ptr : Integer := 1;

   ScrlDefine : String := "SELECT * FROM customers order by customerid limit 100:null:%6s| %-30s| %-20s| %-10s| %-10s:customerid:firstname:lastname:city:state:";

   OrdersDefine : String := "SELECT * FROM orders order by orderdate LIMIT 200:null:%6s| %-12s| %-6s| %10s| %10s| %10s:orderid:orderdate:customerid:netamount:tax:totalamount:";
   SubOrdersDefine : String := "SELECT * FROM cust_hist where customerid = '%s':customerid:%6s| %-6s| %-6s|:customerid:orderid:prod_id:";

   HistDefine : String := "SELECT * FROM cust_hist order by orderid LIMIT 200:null:%6s| %-6s| %-6s|:customerid:orderid:prod_id:";
   SubHistDefine : String := "SELECT * FROM cust_hist where customerid = '%s':customerid:%6s| %-6s| %-6s|:customerid:orderid:prod_id:";

   ProdDefine : String := "SELECT * FROM products order by prod_id:null:%6s| %-30s| %-20s| %-10s| %-10s:prod_id:title:actor:price:common_prod_id:";
   SubProdDefine : String := "SELECT * FROM orders where customerid = '%s':customerid:%6s| %-12s| %-6s| %10s| %10s| %10s:orderid:orderdate:customerid:netamount:tax:totalamount:";


  -- type Menu_Record is record
  --    Prompt : String_Access;
  --    Func :  access procedure;  --Function_Access;
  -- end record;
   type Definition_Type is array (Positive range <>) of String_Access;

--   L_Ack_List : Definition_Type  :=
--     ((new String'("SELECT * FROM customers order by customerid:null:%6s| %-30s| %-20s| %-10s| %-10s:customerid:firstname:lastname:city:state:")),
--      (new String'("SELECT * FROM cust_hist where customerid = '%s':customerid:%6s| %-6s| %-6s|:customerid:orderid:prod_id:")),
--      (new String'("SELECT * FROM products where prod_id = '%s':prod_id:%6s| %-30s| %-20s| %-10s| %-10s:prod_id:title:actor:price:common_prod_id:")),
--      (new String'("SELECT * FROM products where common_prod_id = '%s':common_prod_id:%6s| %-30s| %-20s| %-10s| %-10s:prod_id:title:actor:price:common_prod_id:"))
--     );
    --(new String'("SELECT * FROM orders where customerid = '%s':customerid:%6s| %-12s| %-6s| %10s| %10s| %10s:orderid:orderdate:customerid:netamount:tax:totalamount:")),

   L_Ack_List : Definition_Type  :=
     ((new String'("SELECT * FROM ships WHERE shipowner = '%s' order by ship_id:xtend:%10s| %-30s| %-30s|:ship_id:ship_name:captain:")),
     (new String'("SELECT * FROM ships WHERE shipowner = '%s' order by ship_id:shipowner:%10s| %-30s| %-30s|:ship_id:ship_name:captain:"))
     );


   type Days_of_Week is (Sunday,
                         Monday,
                         Tuesday,
                         Wednesday,
                         Thursday,
                         Friday,
                         Saturday);
   package Ada_Format is
     new Formatter (Enumerated => Days_of_Week);
   use     Ada_Format; -- Direct visibility of F conversion functions


   package FieldsVector is new Ada.Containers.Vectors (Natural,
                                                       Unbounded_String);
   use FieldsVector;


   procedure Split (InVector : in out FieldsVector.Vector; InString : in String);



   function Fld (CI : Direct_Cursor; FldNme : Unbounded_String) return String is
   begin
      for i in 0..CI.Field_Count-1 loop
         if CI.Field_Name(i) = To_String(FldNme) then
            return CI.Value(i);
         end if;
      end loop;
      Display_Warning.Warning("No Field " & To_String(FldNme));
      return To_String(FldNme) & " Not Found";
   end;

   function Fld (CI : Direct_Cursor; FldNme : String) return String is
   begin
      return Fld(CI,To_Unbounded_String(FldNme));
   end Fld;



   function FieldsToValues (CI : in out Direct_Cursor; InVector : in out FieldsVector.Vector) return Values is
      retArray : Values(1..Integer(InVector.Length)-3); -- := (1 => Value);
   begin

      for i in 1..Integer(InVector.Length)-3 loop
         retArray(i) :=  F(Fld(CI,InVector(i+2)));
      end loop;

      return retArray;

   end;


   procedure Read_Scroll (Scrl_Buffer : in out Scrl_List.List;
                          SQLstatement :String;
                          CI : in out Direct_Cursor;
                          Fields : in out FieldsVector.Vector) is

      Stmt : Prepared_Statement;
      targx,targy,targz,distance : Long_Long_Float;

      scratch : Unbounded_String;
   begin

      Clear(Scrl_Buffer);

      Stmt:= Prepare (SQLstatement, Index_By => Field_Index'First);

      CI.Fetch (DB, Stmt);

      if CI.Has_Row then
         CI.First;
         while CI.Has_Row loop

            if Radar_Mode then
               targx := Long_Long_Float'Value(Fld(CI,"loc_x"));
               targy := Long_Long_Float'Value(Fld(CI,"loc_y"));
               targz := Long_Long_Float'Value(Fld(CI,"loc_z"));

               distance := Value_Functions.Sqrt (((MyLocX - targx)**2) + ((MyLocY-targy)**2) + ((MyLocZ-targz)**2)) ;

               --  scratch := To_Unbounded_String("");
             --  scratch := scratch & distance'Image &" ";

               scratch := To_Unbounded_String(Ada_Format.SPut ("%f ",F(Float(distance))));


            else
               scratch := To_Unbounded_String("");
            end if;

            scratch := scratch & To_Unbounded_String(Ada_Format.SPut (To_String(Fields(2)),
                                                     FieldsToValues(CI,Fields)));
            Scrl_Buffer.Append(New_Item => (  CI.Current   , scratch  ));

          --  Scrl_Buffer.Append(New_Item => (  CI.Current   ,
          --                                   To_Unbounded_String(Ada_Format.SPut (To_String(Fields(2)),
          --                                     FieldsToValues(CI,Fields)))  ));

            CI.Next;
         end loop;
         CI.First;
      end if;
   end Read_Scroll;


   procedure Split (InVector : in out FieldsVector.Vector; InString : in String) is
      Cursor : Integer;
      scratch : Unbounded_String := To_Unbounded_String(InString);
      AppendStr : Unbounded_String;
   begin

      Cursor := Index(scratch,":");

      if Cursor /= 0 then
         while  Cursor /= 0 loop
            AppendStr := To_Unbounded_String(Slice (Source => scratch,Low => 1,High => Cursor-1));
            if Length(AppendStr) > 0 then
               InVector.Append(AppendStr);
            end if;

            Delete(scratch,1,Cursor);
            Cursor := Index(scratch,":");
            if Cursor = 0 and then Length(scratch) > 0 then
               InVector.Append(scratch);
            end if;
         end loop;
      else
         if Length(scratch) > 0 then
            InVector.Append(scratch);
         end if;

      end if;

   end Split;


   function OpenDb return Boolean is
      IsOpen   : Boolean;
   begin
    --  DB_Descr := Setup(Database => "dellstore2",
      DB_Descr := Setup(Database => "universe",
                        User => "postgres",
                        Host => "10.10.0.129",
                        Password => "",
                        Port => 5432
                       );
      DB := DB_Descr.Build_Connection;

      DB_Background := DB_Descr.Build_Connection;
      DB_Damage := DB_Descr.Build_Connection;
      DB_Torpedo := DB_Descr.Build_Connection;

      IsOpen := DB.Check_Connection;
      if IsOpen then
         Put_Line("Connection is open.");
      else
         Put_Line("Last Db error = " & DB.Error);
      end if;
      return IsOpen;
   end OpenDb;


   procedure CloseDb is
   begin

      -- reset state of connection for reuse
      Reset_Connection(DB);
      Free (DB);

      Reset_Connection(DB_Background);
      Free (DB_Background);

      Reset_Connection(DB_Damage);
      Free (DB_Damage);

      Reset_Connection(DB_Torpedo);
      Free (DB_Torpedo);

      Free (DB_Descr);

   end CloseDb;


   package Display_Form is new Templates; --  moved to .ads

   Relation_Field : Unbounded_String;

   procedure Scroll (L_AckStatement : String; Down : Integer := 0; Left : Integer := 0; AltFunctions : Boolean := False) is
      c : Key_Code;
     -- FindElement : Scrl_Record;
      SaveID : Integer;
      CI : Direct_Cursor;
      CurrentLine : Line_Position := 0;
      CurrentCurs : Scrl_List.Cursor;
      TopLine : Line_Position;
      TermLnth : Line_Position;
      TermWdth : Column_Position;
      BottomLine : Line_Position ;
      Scrl_Buffer : Scrl_List.List;
      Display_Window : Window;
      DefList,PassList : FieldsVector.Vector;
      SQLQuery : Unbounded_String;

      Regex : GNAT.Regpat.Pattern_Matcher (1024);
      Matches : GNAT.Regpat.Match_Array (0 .. 1);

      FromPattern : String := "FROM\s+(\S+)\s+";
      TableName : Unbounded_String;

      procedure Scroll_Up is
      begin
         Move_Cursor(Display_Window,Line   => TopLine,Column => 0);
         Delete_Line(Display_Window);
         Move_Cursor(Display_Window,Line   => BottomLine,Column => 0);
         Insert_Line(Display_Window);
         Box(Display_Window);
         Refrosh(Display_Window);
      end Scroll_Up;

      procedure Scroll_Down is
      begin
         Move_Cursor(Display_Window,Line   => BottomLine,Column => 0);
         Delete_Line(Display_Window);
         Move_Cursor(Display_Window,Line   => TopLine,Column => 0);
         Insert_Line(Display_Window);
         Box(Display_Window);
         Refrosh(Display_Window);
      end Scroll_Down;

      Procedure Clear_Region is
      begin
         for i in TopLine .. BottomLine loop
            Move_Cursor(Display_Window,Line   => i,Column => 2);
            Clear_To_End_Of_Line(Display_Window);
         end loop;
         -- CurrentLine := 0;
      end Clear_Region;



      procedure Increment (IncLine : in out Line_Position) is
      begin
         if TopLine + Incline < BottomLine then
            IncLine := IncLine + 1;
         else
            Scroll_Up;
         end if;
      end Increment;

      procedure Decrement (IncLine : in out Line_Position) is
      begin
         if Incline > 0 then
            IncLine := IncLine - 1;
         else
            Scroll_Down;
         end if;
      end Decrement;

      procedure HiLite (Win : Window; Prompt : Unbounded_String; Line_Num : Line_Position) is
      begin
         Set_Character_Attributes(Win, (Reverse_Video => True,others => False));
         Add (Win => Win,
              Line => Line_Num,
              Column => 2,
              Str => To_String(Prompt));
         Refrosh(Win);
         Set_Character_Attributes(Win, Normal_Video);
      end HiLite;

      procedure LoLite (Win : Window; Prompt : Unbounded_String; Line_Num : Line_Position) is
      begin
         Set_Character_Attributes(Win,Attr => Normal_Video);
         Add (Win => Win,
              Line => Line_Num,
              Column => 2,
              Str => To_String(Prompt));
         Refrosh(Win);
      end LoLite;

      function Count_Back(Csr : Scrl_List.Cursor) return integer is
         CountCsr : Scrl_List.Cursor := Csr;
         Counter : Integer := 1;
      begin
         loop
            exit when CountCsr = Scrl_Buffer.First;
            Scrl_List.Previous(CountCsr);
            Counter := Counter +1;
         end loop;
         return Counter;
      end Count_Back;

      procedure Redraw_Screen (Win : Window; Message : Unbounded_String :=To_Unbounded_String("")) is
         curs2 : Scrl_List.Cursor;
         LineNum : Line_Position := 0;
         Heading : Unbounded_String := SQLQuery; -- DefList(0); --Message;
      begin



         Clear_Region;
         if not Scrl_Buffer.Is_Empty then
            curs2 := CurrentCurs;

            for i in 1 .. CurrentLine loop
               if curs2 /= Scrl_Buffer.First then
                  Scrl_List.Previous(curs2);
               end if;
            end loop;

            while curs2 /= Scrl_Buffer.Last loop

               Add(Win,Line => TopLine + LineNum,Column => 2,Str => To_String(Element(curs2).Prompt) );
               Clear_To_End_Of_Line(Win);

               Scrl_List.Next(curs2);

               LineNum := LineNum +1;
               exit when LineNum+ TopLine >= BottomLine;
            end loop;
            Add(Win,Line => TopLine + LineNum,Column => 2,Str => To_String(Element(curs2).Prompt) );
            Clear_To_End_Of_Line(Win);

            if SU.Length(Heading) > Integer(TermWdth)-2 then
               Heading := To_Unbounded_String(SU.Slice(Heading,1,Integer(TermWdth)-2));
            end if;

         end if;
         Add (Win => Display_Window,
              Column => Column_Position((Integer(TermWdth) - SU.Length(Heading)) / 2),
              Line => 2,
              Str => To_String(Heading));
         if AltFunctions then
            Add (Win,Line => TermLnth - 2,
                 Column => 1, Str => "F5 ReRead  F6 Lasers  F7 Torpedo  ESC exit");
         else

            Add (Win,Line => TermLnth - 2,
                 Column => 1, Str => "F2 Edit  F3 Activate  F4 New Ship  F5 ReRead  ESC exit ");
         end if;
            Clear_To_End_Of_Line(Win);
         Box(Win);

      end Redraw_Screen;


      Width : Column_Position :=  60;
      Length : Line_Position := 20;

     -- package Drack is new Dbase.DrackSpace;


   begin



      Clear(DefList);

      Split(DefList,L_AckStatement);

      if DefList(1) = "null" then

         SQLQuery := DefList(0);
      else
         SQLQuery := To_Unbounded_String(Ada_Format.SPut(To_String(DefList(0)),F(To_String(Relation_Field))));

      end if;


      -- extract TableName from the SQL query by parsing out  FROM tablename by regular expression.
      GNAT.Regpat.Compile (Regex, FromPattern);
      GNAT.Regpat.Match (Regex, To_String(SQLQuery) , Matches);
      if Matches (0) /= GNAT.Regpat.No_Match then
          TableName := To_Unbounded_String(SU.Slice(SQLQuery,Matches(1).First,Matches(1).Last));
      else
         Display_Warning.Warning("No FROM in SQL");
      end if;


      Read_Scroll(Scrl_Buffer,To_String(SQLQuery),CI,DefList);

   --   if  CI.Has_Row then --not  Scrl_Buffer.Is_Empty then
         Get_Size(Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
      if not Scrl_Buffer.Is_Empty then

         declare
               promptlength : Integer;
         begin

            CurrentCurs := Scrl_Buffer.First;

            while CurrentCurs /= Scrl_Buffer.Last loop
               promptlength := SU.Length(Element(CurrentCurs).Prompt) + 5;
               if promptlength > Integer(Width) then
                  Width := Column_Position(promptlength);
               end if;
               Scrl_List.Next(CurrentCurs);
            end loop;
            promptlength := SU.Length(Element(CurrentCurs).Prompt) + 5;
            if promptlength > Integer(Width) then
               Width := Column_Position(promptlength);
            end if;

         end;



        -- Width := Column_Position(SU.Length(Element(Scrl_Buffer.First).Prompt) + 5);
      else
         Width := 90;
      end if;

         if Width < TermWdth then

            Display_Window := Sub_Window(Win => Standard_Window,
                                         Number_Of_Lines => Length,
                                         Number_Of_Columns => Width,
                                         First_Line_Position => ((TermLnth - Length) / 2)+Line_Position(Down),
                                         First_Column_Position => ((TermWdth - Width) / 2)-Column_Position(Left));

            Clear(Display_Window);
            Box(Display_Window);
            Refrosh(Display_Window);

            -- Clear;

            Get_Size(Display_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
            TopLine := 4;
            BottomLine := TermLnth - 4;
            CurrentLine := 0;




            CurrentCurs := Scrl_Buffer.First;

            Redraw_Screen(Display_Window);

            Refrosh(Display_Window);

            loop
            if not Scrl_Buffer.Is_Empty then
               HiLite(Display_Window,Element(CurrentCurs).Prompt,CurrentLine+TopLine);
            end if;

               c := Get_Keystroke;

            exit when Dbase.ShipDestroyed;

               if c in Special_Key_Code'Range then
                  case c is
                  when Key_F2 =>
                     if CI.Has_Row then
                        SaveID := Element(CurrentCurs).ID;
                        CI.Absolute(Element(CurrentCurs).ID);
                     end if;

                     if Fld(CI,To_Unbounded_String("shipowner")) = UserLoggedUserId then

                     if SU.Length(TableName) /= 0 then

                        if Display_Form.Initialise(CI,To_String(TableName))  then
                           Display_Form.Edit_Page;
                           -- Display_Form.Command_Screen;
                        end if;
                     else
                        Display_Warning.Warning("No FROM in SQL");
                     end if;

                     if Display_Form.Current_Record_Updated then
                        Read_Scroll(Scrl_Buffer,To_String(SQLQuery),CI,DefList);
                        CurrentCurs := Scrl_Buffer.First;
                        while CurrentCurs /= Scrl_Buffer.Last loop
                           exit when Element(CurrentCurs).ID = SaveID;
                           CurrentCurs := Scrl_List.Next(CurrentCurs);
                        end loop;
                     end if;
                     else
                        Display_Warning.Warning("Cannot edit a ship not yours");
                     end if;

                     Clear(Display_Window);
                     Redraw_Screen(Display_Window);

                  when Key_F3 =>
                     if not AltFunctions then
                        if CI.Has_Row then
                           SaveID := Element(CurrentCurs).ID;
                           CI.Absolute(Element(CurrentCurs).ID);
                        end if;

                        if Fld(CI,To_Unbounded_String("shipowner")) = UserLoggedUserId then

                           if SU.Length(TableName) /= 0 then
                              if Display_Form.Initialise(CI,To_String(TableName))  then
                                 Display_Form.Command_Screen;
                              end if;
                           else
                              Display_Warning.Warning("No FROM in SQL");
                           end if;

                           -- if Display_Form.Current_Record_Updated then
                           Read_Scroll(Scrl_Buffer,To_String(SQLQuery),CI,DefList);
                           CurrentCurs := Scrl_Buffer.First;
                           while CurrentCurs /= Scrl_Buffer.Last loop
                              exit when Element(CurrentCurs).ID = SaveID;
                              CurrentCurs := Scrl_List.Next(CurrentCurs);
                           end loop;
                           -- end if;
                        else
                           Display_Warning.Warning("Cannot pilot a ship not yours");
                        end if;

                        Clear(Display_Window);
                        Redraw_Screen(Display_Window);
                     end if;


                  when Key_F4 =>
                     if not AltFunctions then
                        if CI.Has_Row then
                           SaveID := Element(CurrentCurs).ID;
                           CI.Absolute(Element(CurrentCurs).ID);
                        end if;


                        if SU.Length(TableName) /= 0 then
                           if Display_Form.Initialise(CI,To_String(TableName),True)  then

                              -- Dbase.DrackSpace.Initialise_Defaults;
                              -- Drack.Initialise_Defaults;

                              Display_Form.Set_Default("shipowner",To_String(UserLoggedUserId));
                              Display_Form.Set_Default("loc_x","0");
                              Display_Form.Set_Default("loc_y","0");
                              Display_Form.Set_Default("loc_z","0");
                              Display_Form.Set_Default("dest_x","0");
                              Display_Form.Set_Default("dest_y","0");
                              Display_Form.Set_Default("dest_z","0");

                              Display_Form.Set_Default("navcom_funct","100");
                              Display_Form.Set_Default("navcom_oprt","100");
                              Display_Form.Set_Default("navcom_reli","100");
                              Display_Form.Set_Default("navcom_energ","5");

                              Display_Form.Set_Default("jmpeng_funct","100");
                              Display_Form.Set_Default("jmpeng_oprt","100");
                              Display_Form.Set_Default("jmpeng_reli","100");
                              Display_Form.Set_Default("jmpeng_energ","50000");

                              Display_Form.Set_Default("engine_funct","100");
                              Display_Form.Set_Default("engine_oprt","100");
                              Display_Form.Set_Default("engine_reli","100");
                              Display_Form.Set_Default("engine_energ","10000");

                              Display_Form.Set_Default("deflect_funct","100");
                              Display_Form.Set_Default("deflect_oprt","100");
                              Display_Form.Set_Default("deflect_reli","100");
                              Display_Form.Set_Default("deflect_energ","10000");

                              Display_Form.Set_Default("hull_value","1000");
                              Display_Form.Set_Default("fuel_value","500000000");
                              -- Display_Form.Set_Default("torp_lock","0");

                              Display_Form.Edit_Page;
                           end if;
                        else
                           Display_Warning.Warning("No FROM in SQL");
                        end if;

                        if Display_Form.Current_Record_Updated then
                           Read_Scroll(Scrl_Buffer,To_String(SQLQuery),CI,DefList);
                           CurrentCurs := Scrl_Buffer.First;
                           while CurrentCurs /= Scrl_Buffer.Last loop
                              exit when Element(CurrentCurs).ID = SaveID;
                              CurrentCurs := Scrl_List.Next(CurrentCurs);
                           end loop;
                        end if;


                        Clear(Display_Window);
                        Redraw_Screen(Display_Window);
                        Refrosh(Display_Window);
                     end if;

                  when Key_F5 =>
                     if CI.Has_Row then
                        SaveID := Element(CurrentCurs).ID;
                        CI.Absolute(Element(CurrentCurs).ID);
                     end if;

                     -- if Display_Form.Current_Record_Updated then
                     Read_Scroll(Scrl_Buffer,To_String(SQLQuery),CI,DefList);
                     CurrentCurs := Scrl_Buffer.First;
                     while CurrentCurs /= Scrl_Buffer.Last loop
                        exit when Element(CurrentCurs).ID = SaveID;
                        CurrentCurs := Scrl_List.Next(CurrentCurs);
                     end loop;
                     -- end if;

                     Clear(Display_Window);
                     Redraw_Screen(Display_Window);
                  when Key_F6 =>
                     if CI.Has_Row then
                        SaveID := Element(CurrentCurs).ID;
                        CI.Absolute(Element(CurrentCurs).ID);
                     end if;

                     if AltFunctions then
                       -- if Display_Form.Initialise(CI,To_String(TableName),NoWindow => True)  then

                        Display_Form.Fire_Lasers(To_Unbounded_String(Fld(CI,To_Unbounded_String("ship_id")))
                                                   );
                       -- end if;

                        --   Read_Scroll(Scrl_Buffer,To_String(SQLQuery),CI,DefList);
                     --   CurrentCurs := Scrl_Buffer.First;
                     --   while CurrentCurs /= Scrl_Buffer.Last loop
                     --      exit when Element(CurrentCurs).ID = SaveID;
                     --      CurrentCurs := Scrl_List.Next(CurrentCurs);
                     --   end loop;


                        Clear(Display_Window);
                        Redraw_Screen(Display_Window);
                     else
                        Display_Warning.Warning("Damage Only From Radar");
                     end if;
                  when Key_F7 =>
                     if CI.Has_Row then
                        SaveID := Element(CurrentCurs).ID;
                        CI.Absolute(Element(CurrentCurs).ID);
                     end if;

                     if AltFunctions then
                        declare

                           procedure Fire_Tube1 is
                           begin
                              Display_Form.Fire_Torpedo(To_Unbounded_String(Fld(CI,To_Unbounded_String("ship_id"))));
                           end Fire_Tube1;

                           procedure Fire_Tube2 is
                           begin
                              Display_Form.Fire_Tube2_Torpedo(To_Unbounded_String(Fld(CI,To_Unbounded_String("ship_id"))));
                           end Fire_Tube2;


                           Torpedos : Process_Menu.Menu_Type  :=
                             ((new String'("Fire Tube 1"),Fire_Tube1'Unrestricted_Access),
                              (new String'("Fire Tube 2"),Fire_Tube2'Unrestricted_Access));
                        begin

                           Process_Menu.Open_Menu(5,Torpedos);

                        end;

                       -- Display_Form.Fire_Torpedo(To_Unbounded_String(Fld(CI,To_Unbounded_String("ship_id"))));


                        Read_Scroll(Scrl_Buffer,To_String(SQLQuery),CI,DefList);
                        CurrentCurs := Scrl_Buffer.First;
                        while CurrentCurs /= Scrl_Buffer.Last loop
                           exit when Element(CurrentCurs).ID = SaveID;
                           CurrentCurs := Scrl_List.Next(CurrentCurs);
                        end loop;

                        Clear(Display_Window);
                        Redraw_Screen(Display_Window);

                     end if;


                  when Key_Cursor_Down =>
                     if (CurrentCurs /= Scrl_Buffer.Last) then
                        LoLite(Display_Window,Element(CurrentCurs).Prompt,CurrentLine+TopLine);
                        Increment(CurrentLine);
                        Scrl_List.Next(CurrentCurs);
                     end if;
                  when Key_Cursor_Up =>
                     if (CurrentCurs /= Scrl_Buffer.First) then
                        LoLite(Display_Window,Element(CurrentCurs).Prompt,CurrentLine+TopLine);
                        Decrement(CurrentLine);
                        Scrl_List.Previous(CurrentCurs);
                     end if;
                  when Key_Next_Page =>
                     for i in 0 .. BottomLine-TopLine loop
                        if CurrentCurs /= Scrl_Buffer.Last then
                           Scrl_List.Next(CurrentCurs);
                        end if;
                     end loop;
                     Redraw_Screen(Display_Window);
                  when Key_Previous_Page =>
                     for i in 0 .. BottomLine-TopLine loop
                        if CurrentCurs /= Scrl_Buffer.First then
                           Scrl_List.Previous(CurrentCurs);
                        end if;
                     end loop;
                     if Line_Position(Count_Back(CurrentCurs)) < BottomLine-TopLine then
                        CurrentLine := 0;
                     end if;
                     Redraw_Screen(Display_Window);
                  when Key_Home =>
                     CurrentCurs := Scrl_Buffer.First;
                     CurrentLine := 0;
                     Redraw_Screen(Display_Window);
                  when Key_End =>
                     CurrentCurs := Scrl_Buffer.Last;
                     CurrentLine := BottomLine-TopLine;
                     Redraw_Screen(Display_Window);
                  when Key_Resize =>
                     Get_Size(Display_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
                     BottomLine := TermLnth - 4;
                     Clear;
                     Redraw_Screen(Display_Window);
                     --    when Key_End => exit;
                  when others => null;
                  end case;
               elsif c in Real_Key_Code'Range then

                  -- Ch := Character'Val (c);

                  case Character'Val (c) is
                  when LF | CR =>
                     begin

                        CI.Absolute(Element(CurrentCurs).ID);

                        if CI.Has_Row and then Definition_Ptr < L_Ack_List'Last then
                           Definition_Ptr := Definition_Ptr + 1;
                           Split(PassList,L_Ack_List(Definition_Ptr).all);
                           if PassList(1) /= "null" then

                              Relation_Field := To_Unbounded_String(Fld(CI,PassList(1)));

                           end if;
                           if AltFunctions then
                              Radar_Mode := False;
                           end if;

                           Scroll (L_Ack_List(Definition_Ptr).all);
                           Definition_Ptr := Definition_Ptr -1;

                           if AltFunctions then
                              Radar_Mode := True;
                           end if;

                           Clear(Display_Window);
                           Redraw_Screen(Display_Window);
                        end if;
                        null;
                     end;
                  when ESC => Exit;
                     when others => null;
                  end case;
               end if;

            end loop;

            Clear(Scrl_Buffer);

            Clear(Display_Window);
            Refrosh(Display_Window);

            Delete (Win => Display_Window);
         else
            Display_Warning.Warning("Terminal not wide enough");
         end if;

    --  else
    --    Display_Warning.Warning("No Results for Search");
    --  end if;

   end Scroll;

   procedure Run is

   begin
      if UserLoggedIn then
         if OpenDb then


            Definition_Ptr := 1;

            Relation_Field := UserLoggedUserId;

            Scroll(L_Ack_List(Definition_Ptr).All);

            CloseDb;
         else
            Display_Warning.Warning("No Open SQL Database");
         end if;
      else
         Display_Warning.Warning("You Must be logged In");
      end if;

   end Run;



end Dbase.Scroller;
