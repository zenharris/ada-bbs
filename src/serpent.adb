with Terminal_Interface.Curses;
use  Terminal_Interface.Curses;
with Terminal_Interface.Curses.Text_IO;
use  Terminal_Interface.Curses.Text_IO;
with Terminal_Interface.Curses.Text_IO.Integer_IO;
with Terminal_Interface.Curses.Text_IO.Fixed_IO;



with Ada.Containers.Doubly_Linked_Lists ;
with Ada.Calendar; use Ada.Calendar;
With Ada.Numerics.Float_Random;
use Ada.Numerics.Float_Random;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Message.Post;
with Texaco;


procedure serpent is



   type T_Position is record
      line : Line_Position;
      column : Column_Position ;
   end record;

   function "+" (Left, Right : T_Position) return T_Position is
      res : T_Position;
   begin
      res.line := left.line + right.line;
      res.column := left.column + right.column;
      return res;
   end "+";

   function "-" (Left, Right : T_Position) return T_Position is
      res : T_Position;
   begin
      res.line := left.line - right.line;
      res.column := left.column - right.column;
      return res;
   end "-";

   package P_Lists is new Ada.Containers.Doubly_Linked_Lists(T_Position) ;
   use P_Lists ;

   package Line_Position_Text_IO
   is new Terminal_Interface.Curses.Text_IO.Integer_IO(Line_Position);
   use Line_Position_Text_IO;

   package Column_Position_Text_IO
   is new Terminal_Interface.Curses.Text_IO.Integer_IO(Column_Position);
   use Column_Position_Text_IO;

   package Integer_Text_IO
   is new Terminal_Interface.Curses.Text_IO.Integer_IO(Integer);
   use Integer_Text_IO;

   package Duration_Text_IO
   is new Terminal_Interface.Curses.Text_IO.Fixed_IO(Duration);
   use Duration_Text_IO;

   function Self_Collision(snake : list; pos : T_Position) return Boolean is
   curs : cursor := First(snake);
   begin
      while curs /= Last(snake) loop
         if Element(curs)= pos then
            return true;
         else
            Next(Curs);
         end if;
      end loop;
      return false;
   end Self_Collision;

   procedure Spawn_Point(
      Gen: Generator;
      snake: List;
      point : out T_Position)
   is
   begin
      loop
         point.line := 1 + Line_Position(Float(Lines-2) * Random(Gen))
            mod (Lines-2);
         point.column := 1 + Column_Position(Float(Columns-2) * Random(Gen))
            mod (Columns-2);
         exit when not Contains(snake, point);
      end loop;
      Add(Line=>point.line, Column=>point.column, ch=>'x');
   end Spawn_Point;


   snake : list;
   snake_cursor : cursor;
   pos, pop, point : T_position;
   dir, last_dir : T_position :=(-1,0); --snake direction
   Gen : Generator;
   temps : Time := Clock;
   duree : Duration := 1.0;
   key : Real_Key_Code;
   curs_visibility : Cursor_Visibility := Invisible;
   speed : Float := 2.0;
   loop_delay : Duration := Duration(1.0/speed);
   score : Integer := 0;
begin
 --  Init_Screen;
   Set_Timeout_Mode(mode=>Non_Blocking,Amount=>0);
   Set_Cursor_Visibility(curs_visibility);
   Set_Echo_Mode(SwitchOn => false);
   Set_KeyPad_Mode(SwitchOn => true);
   Clear;
   border(standard_window);
   Move_cursor(standard_window,0,1);
   Put("jeu du serpent");
   Move_Cursor(standard_window,0,18);
   Put("score : ");
   Put(score);

   --init the snake with a size
   pos := (line=>Lines/2,Column=>Columns/2);
   for i in 1..8 loop
      Prepend(snake,pos);
      Add(Line=>pos.line, Column=>pos.column, ch=>'O');
   end loop;
   Spawn_Point(Gen, snake, point);
   --star moving the snake
   temps := Clock;
   main: loop
      --Get the direction
      begin
         key := Get_Keystroke;
         case key is
            when KEY_UP =>
               if last_dir /= (1,0) then
                  dir := (-1,0);
               end if;
            when KEY_DOWN =>
               if last_dir /= (-1,0) then
                  dir := (1,0);
               end if;
            when KEY_LEFT =>
               if last_dir /= (0,1) then
                  dir := (0,-1);
               end if;
            when KEY_RIGHT =>
               if last_dir /= (0,-1) then
                  dir := (0,1);
               end if;
            when others => null;
         end case;
      end;
      --Moving the snake on tick
      if (Clock - temps) >= loop_delay then
         temps := Clock;
         last_dir := dir;
         pos := pos + dir;
         --detect collision with border
         exit when pos.column > (Columns - 2);
         exit when pos.column < (1);
         exit when pos.line > (Lines - 2);
         exit when pos.line < (1);
         --detect self collision
         exit when Self_Collision(snake, pos) ;
         --detect if snake eat the point
         if pos = point then
            Spawn_Point(Gen, snake, point);
            Append(snake, snake.Last_Element);
            speed := speed + 2.0;
            loop_delay := Duration(1.0/speed);
            Move_Cursor(standard_window,Lines-1,1);
            Put("loop_delay ");
            Put(loop_delay);
            Refresh;
            score := score + 1;
            Move_Cursor(standard_window,0,18);
            Put("score : ");
            Put(score);
         end if;
         --Snake moves and draw
         declare
            curs,c_next,c_prev : Cursor;
            cur,next,prev : T_Position;
            a,b,c : T_Position;
         begin
            --the head of the snake
            Prepend(snake,pos);
            curs := First(snake);
            cur := Element(curs);
            Add(
               Line=>Element(curs).line,
               Column=>Element(curs).column,
               ch=>'O');
            --drawing the body part just after the head
            curs := P_lists.Next(curs);
            if Has_Element(curs) then
               cur := Element(curs);
               c_next := P_Lists.Next(curs);
               next := Element(P_Lists.Next(curs));
               prev := Element(P_Lists.Previous(curs));
               a := cur - prev;
               b := cur - next;
               c := a + b;
               if c = (-1,-1) then
                  Add(
                     Line=>Element(curs).line,
                     Column=>Element(curs).column,
                     ch=>ACS_MAP(ACS_Upper_Left_Corner));
               elsif c = (1,-1) then
                  Add(
                     Line=>Element(curs).line,
                     Column=>Element(curs).column,
                     ch=>ACS_MAP(ACS_Lower_Left_Corner));
               elsif c = (-1,1) then
                  Add(
                     Line=>Element(curs).line,
                     Column=>Element(curs).column,
                     ch=>ACS_MAP(ACS_Upper_Right_Corner));
               elsif c = (1,1) then
                  Add(
                     Line=>Element(curs).line,
                     Column=>Element(curs).column,
                     ch=>ACS_MAP(ACS_Lower_Right_Corner));
               elsif a.line = 0  then
                  Add(
                     Line=>Element(curs).line,
                     Column=>Element(curs).column,
                     ch=>ACS_MAP(ACS_Horizontal_Line));
               elsif a.column = 0 then
                  Add(
                     Line=>Element(curs).line,
                     Column=>Element(curs).column,
                     ch=>ACS_MAP(ACS_Vertical_Line));
               end if;
            end if;

            Refresh;
            --delete element at the end
            pop := Last_element(snake);
            Delete_Last(snake);
            if (not Contains(snake, pop)) then
               Add(Line=>pop.line, Column=>pop.column, ch=>' ');
            end if;
         end;
         Refresh;
      end if;
   end loop main;
   Move_Cursor(line=>Lines/2,column=>(Columns-10)/2);
   Put("Game Over!");

   while Get_Keystroke in Special_Key_Code'Range loop
      null;
   end loop;

   Set_Timeout_Mode(mode=>Blocking,Amount=>0);
   curs_visibility := Very_Visible; --  Invisible;
   Set_Cursor_Visibility(curs_visibility);


   declare
      package SUIO renames Ada.Text_IO.Unbounded_IO;
      package SU renames Ada.Strings.Unbounded;
      package SF renames Ada.Strings.Fixed;


      FileName, Nick : Unbounded_String;
      File : Ada.Text_IO.File_Type;
      Column : Column_Position := (Columns-35)/2;
      PostDate : Time := Clock;
   begin
      Add (Line => Lines/2,Column => Column,Str => "Enter You Nick : ");
      Clear_To_End_Of_Line;
      loop
         Texaco.Line_Editor(Standard_Window,
                            StartLine => Lines / 2,
                            StartColumn => Column + 18,
                            Editlength => 16,
                            Edline => Nick,
                            MaxLength => 15);
         exit when Nick /= "";
      end loop;

      Filename := "serpent/"& Message.Post.Pad(Integer'Image(score),3)&"-"&Message.Post.Generate_UID&".score";
      Ada.Text_IO.Create (File => File,
              Mode => Ada.Text_IO.Out_File,
              Name => To_String(FileName));

      SUIO.Put_Line(File,"Nick: " & Nick);
      SUIO.Put_Line(File,"Score:" & To_Unbounded_String(Integer'Image(score)));
      SUIO.Put_Line(File,"Date: " & To_Unbounded_String(Image (PostDate)));
      Ada.Text_IO.Close(File);
   end;



   -- Display_Warning.Warning("Game Over!");
   --key := Get_Keystroke;
   -- End_Windows;
end serpent;
