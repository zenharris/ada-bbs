with Display_Warning;

package body Dbase.Scroller is
   DB_Descr : Database_Description;
   DB       : Database_Connection;

   Definition_Ptr : Integer := 1;

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

   Definition_List : Definition_Type  :=
     ((new String'("SELECT * FROM customers order by customerid:null:%6s| %-30s| %-20s| %-10s| %-10s:customerid:firstname:lastname:city:state:")),
      --(new String'("SELECT * FROM orders where customerid = '%s':customerid:%6s| %-12s| %-6s| %10s| %10s| %10s:orderid:orderdate:customerid:netamount:tax:totalamount:")),
      (new String'("SELECT * FROM cust_hist where customerid = '%s':customerid:%6s| %-6s| %-6s|:customerid:orderid:prod_id:")),

      --(new String'("SELECT * FROM orders where orderid = '%s':orderid:%6s| %-12s| %-6s| %10s| %10s| %10s:orderid:orderdate:customerid:netamount:tax:totalamount:"))
      (new String'("SELECT * FROM products where prod_id = '%s':prod_id:%6s| %-30s| %-20s| %-10s| %-10s:prod_id:title:actor:price:common_prod_id:")),
      (new String'("SELECT * FROM products where common_prod_id = '%s':common_prod_id:%6s| %-30s| %-20s| %-10s| %-10s:prod_id:title:actor:price:common_prod_id:"))
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
      CIB :Direct_Cursor := CI;
   begin
      for i in 0..CI.Field_Count-1 loop
         if CI.Field_Name(i) = To_String(FldNme) then
            return CI.Value(i);
         end if;
      end loop;
      Display_Warning.Warning("No Field " & To_String(FldNme));
      return To_String(FldNme) & " Not Found";
   end;

   function FieldsToValues (CI : in out Direct_Cursor; InVector : in out FieldsVector.Vector) return Values is
      retArray : Values(1..Integer(InVector.Length)-3); -- := (1 => Value);
   begin

      for i in 1..Integer(InVector.Length)-3 loop
         retArray(i) :=  F(Fld(CI,InVector(i+2)));
      end loop;

      return retArray;

   end;

  --  procedure Show_Scroll (Definition :String;SubDefinition : String;CIP : Direct_Cursor) is

   Current_Cursor : Direct_Cursor;

   procedure Read_Scroll (Scrl_Buffer : in out Scrl_List.List; SQLstatement :String;CI : in out Direct_Cursor;
                         Fields : in out FieldsVector.Vector) is
      -- CI : Direct_Cursor;
      Stmt : Prepared_Statement;
      -- Fields : FieldsVector.Vector;
      DepthFlag :Boolean := False;
      scratch : Unbounded_String;

      Crnt : Integer := 1;

   begin

      Clear(Scrl_Buffer);
   --   Clear(Fields);

    --  Split(Fields,Definition);

  --    if Fields(1) = "null" then

  --     Stmt:= Prepare (To_String(Fields(0)),
  --            Use_Cache => True, Index_By => Field_Index'First);
   ---   else
  --       DepthFlag := True;
  --       Add (Standard_Window,Line => 1,Column => 1, Str => To_String(Fields(1)));
  --       refresh;
  --       scratch := To_Unbounded_String(Ada_Format.SPut(To_String(Fields(0)),F(Fld(Current_Cursor,Fields(1)))));
  --       Add (Standard_Window,Line => 2,Column => 1, Str => To_String(scratch));
  --       Refresh;
  --       Stmt := Prepare (To_String(scratch),Use_Cache => True, Index_By => Field_Index'First );

  --    end if;

         Stmt:= Prepare (SQLstatement, Index_By => Field_Index'First);



      CI.Fetch (DB, Stmt);
      -- CI.Find (Id); -- Find record by Id
      if CI.Has_Row then
         CI.First;
         while CI.Has_Row loop

            --      for i in 0..CI.Field_Count-1 loop
            --      --   Put_Line (CI.Value(i));
            --         Put_Line (CI.Field_Name(i) &" "& CI.Value(i));
            --      end loop;


            Scrl_Buffer.Append(New_Item => ( Crnt   ,
                                             To_Unbounded_String(Ada_Format.SPut (To_String(Fields(2)),
                                               FieldsToValues(CI,Fields)))  ));

            -- Ada_Format.Put (To_String(Fields(2)),FieldsToValues(CI,Fields));
            -- Put_Line("");
            -- if not DepthFlag then
            --    Show_Scroll(SubDefinition,SubDefinition,CI);
            -- end if;


            --     SubStmt := Prepare (Ada_Format.SPut(To_String(SubFields(0)),F(Fld(CI,SubFields(1)))),Use_Cache => True );
            --    CIB.Fetch (DB,SubStmt);
            --     CIB.First;
            --     while CIB.Has_Row loop
            --        Ada_Format.Put (To_String(Fields(1)),FieldsToValues(CIB,Fields));
            --        Put_Line("");
            --        CIB.Next;
            --    end loop;


            -- Ada_Format.Put (To_String(Fields(1)),(F(Fld(CI,Fields(2))), F(Fld(CI,Fields(3))),
            --                F(Fld(CI,Fields(4))),F(Fld(CI,Fields(5))), F(Fld(CI,Fields(6)))
            --               ));


            CI.Next;
            Crnt := Crnt + 1;
         end loop;


         CI.First;
         --if not DepthFlag then
        --    CIP := CI;
         --end if;

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




   procedure OpenDb is

      IsOpen   : Boolean;

   begin
      DB_Descr := Setup(Database => "dellstore2",
                        User => "postgres",
                        Host => "10.10.0.129",
                        Password => "",
                        Port => 5432
                       );
      DB := DB_Descr.Build_Connection;
      IsOpen := DB.Check_Connection;
      if IsOpen then
         Put_Line("Connection is open.");
      else
         Put_Line("Last Db error = " & DB.Error);
      end if;
   end OpenDb;


   procedure CloseDb is
   begin

      -- reset state of connection for reuse
      Reset_Connection(DB);

      Free (DB);
      Free (DB_Descr);

   end CloseDb;


  -- procedure Scroll is
  -- begin
     -- Split(Fields,ScrlDefine);

  --    null;
  -- end;

   Pass_String : Unbounded_String;

   procedure Scroll (SQLstatement : String) is
      c : Key_Code;
      -- FindElement : Scrl_Record;
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

      procedure Scroll_Up is
      begin
         Move_Cursor(Display_Window,Line   => TopLine,Column => 0);
         Delete_Line(Display_Window);
         Move_Cursor(Display_Window,Line   => BottomLine,Column => 0);
         Insert_Line(Display_Window);
         Box(Display_Window);
         Refresh(Display_Window);
      end Scroll_Up;

      procedure Scroll_Down is
      begin
         Move_Cursor(Display_Window,Line   => BottomLine,Column => 0);
         Delete_Line(Display_Window);
         Move_Cursor(Display_Window,Line   => TopLine,Column => 0);
         Insert_Line(Display_Window);
         Box(Display_Window);
         Refresh(Display_Window);
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
         Refresh(Win);
         Set_Character_Attributes(Win, Normal_Video);
      end HiLite;

      procedure LoLite (Win : Window; Prompt : Unbounded_String; Line_Num : Line_Position) is
      begin
         Set_Character_Attributes(Win, Normal_Video);
         Add (Win => Win,
              Line => Line_Num,
              Column => 2,
              Str => To_String(Prompt));
         Refresh(Win);
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
         Heading : Unbounded_String := DefList(0); --Message;
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
               Refresh(Win);

               Scrl_List.Next(curs2);

               LineNum := LineNum +1;
               exit when LineNum+ TopLine >= BottomLine;
            end loop;
            Add(Win,Line => TopLine + LineNum,Column => 2,Str => To_String(Element(curs2).Prompt) );
            Clear_To_End_Of_Line(Win);
            Refresh;

            if SU.Length(Heading) > Integer(TermWdth)-2 then
               Heading := To_Unbounded_String(SU.Slice(Heading,1,Integer(TermWdth)-2));
            end if;



            Add (Win => Display_Window,
                 Column => Column_Position((Integer(TermWdth) - SU.Length(Heading)) / 2),
                 Line => 2,
                 Str => To_String(Heading));

            Add (Win,Line => TermLnth - 2,Column => 1, Str => "  Esc to exit");
            Clear_To_End_Of_Line(Win);
            Box(Win);
         end if;
      end Redraw_Screen;


      Width : Column_Position := 90;
      Length : Line_Position := 20;
      SQLQuery : Unbounded_String;
   begin

      -- Display_Warning.Warning("Am Here");

      Clear(DefList);

      Split(DefList,SQLstatement);

      if DefList(1) = "null" then

       SQLQuery := DefList(0);
      else
      --   Add (Standard_Window,Line => 1,Column => 1, Str => To_String(Pass_String));
      --Clear_To_End_Of_Line;
      --refresh;
      --   SQLQuery := To_Unbounded_String(Ada_Format.SPut(To_String(DefList(0)),F(Fld(Current_Cursor,DefList(1)))));
         SQLQuery := To_Unbounded_String(Ada_Format.SPut(To_String(DefList(0)),F(To_String(Pass_String))));

      end if;

    --  Add (Standard_Window,Line => 1,Column => 1, Str => To_String(SQLQuery));
    --  Clear_To_End_Of_Line;
    --  refresh;


      Read_Scroll(Scrl_Buffer,To_String(SQLQuery),CI,DefList);

      if  CI.Has_Row then --not  Scrl_Buffer.Is_Empty then
         Get_Size(Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);

         Width := Column_Position(SU.Length(Element(Scrl_Buffer.First).Prompt) + 5);
         if Width < TermWdth then

            Display_Window := Sub_Window(Win => Standard_Window,
                                         Number_Of_Lines => Length,
                                         Number_Of_Columns => Width,
                                         First_Line_Position => (TermLnth - Length) / 2,
                                         First_Column_Position => (TermWdth - Width) / 2);

            Clear(Display_Window);
            Box(Display_Window);
            Refresh(Display_Window);

            -- Clear;

            Get_Size(Display_Window,Number_Of_Lines => TermLnth,Number_Of_Columns => TermWdth);
            TopLine := 4;
            BottomLine := TermLnth - 4;
            CurrentLine := 0;




            CurrentCurs := Scrl_Buffer.First;

            Redraw_Screen(Display_Window);

            Refresh(Display_Window);

            loop

               HiLite(Display_Window,Element(CurrentCurs).Prompt,CurrentLine+TopLine);

               c := Get_Keystroke;

               if c in Special_Key_Code'Range then
                  case c is
                  when Key_F2 =>
                     --      FindElement := Element(CurrentCurs);
                     ---      Process_Menu.Open_Menu (Function_Number => 2,Menu_Array => MessageMenu );
                     ---     CurrentCurs := Directory_Buffer.Find(Item => FindElement);
                     --      if CurrentCurs = No_Element then
                     --         CurrentCurs := Directory_Buffer.Last;
                     --      end if;


                     Clear(Display_Window);
                     Redraw_Screen(Display_Window);
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

                    --    CI.Find (Natural(Element(CurrentCurs).ID));
                  --      Current_Cursor := CI;
                  --      Current_Cursor.Find(Element(CurrentCurs).ID);

                        CI.Absolute(Element(CurrentCurs).ID);

                        if CI.Has_Row and then Definition_Ptr < Definition_List'Last then
                           Definition_Ptr := Definition_Ptr + 1;
                           Split(PassList,Definition_List(Definition_Ptr).all);
                           if PassList(1) /= "null" then
                              -- Display_Warning.Warning("Am Here 2");

                              Pass_String := To_Unbounded_String(Fld(CI,PassList(1)));
                             -- Display_Warning.Warning(To_String(Pass_String));
                           end if;

                           Scroll (Definition_List(Definition_Ptr).all);
                           Definition_Ptr := Definition_Ptr -1;
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
         else
            Display_Warning.Warning("Terminal not wide enough");
         end if;

      else
        Display_Warning.Warning("No Results for Search");
      end if;

   end Scroll;

   procedure Run is
      CI : Direct_Cursor;
   begin

      OpenDb;

      Definition_Ptr := 1;

      Scroll(Definition_List(Definition_Ptr).All); --ScrlDefine,CI);
      CloseDb;
   end Run;



end Dbase.Scroller;
