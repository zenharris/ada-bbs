separate (Formatter.Get)
   procedure Format_character (Data          : in     Contents;
                               In_The_String : in out String;
                               Location      : in out Natural;
                               Width         : in     Natural := 0;
                               Left_Justify  : in     Boolean := False) is

      -- ++
      --
      -- FUNCTIONAL DESCRIPTION:
      --
      --         This procedure formats a character and places it in the output string at
      --        the specified location. The character may be left or right-justified in
      --        a field of a specified width.
      --
      -- FORMAL PARAMETERS:
      --
      --         Data:
      --          The variant record containing character to format.
      --
      --      In_The_String:
      --          The output string in which to place the formatted character.
      --
      --      Location:
      --          The position in the output string to place the formatted character.
      --
      --      Width:
      --          The field width alloted to the formatted character.
      --
      --      Left_Justify:
      --          Logical (Boolean) switch.
      --
      -- DESIGN:
      --
      --         If the input Width parameter is zero, then set the output field width to
      --        one, otherwise set it to the specified width.
      --
      --        If the field width is equal to one, then move the input character into
      --        the output string; otherwise, move the input character into output
      --        string field and left-justify it if required.
      --
      -- EXCEPTIONS:
      --
      --         If constraint error is detected, try to place the character in a
      --        a default width output field.
      --
      -- KEYWORDS:
      --
      --         Character, Justify
      --
      -- --

      -- Local constant declarations
      Blanks : constant String(1..255) := (others => ' ');

      -- Local variables
      Field_Width : Natural;

   begin

      -- Set the field width
      if Width = 0 then
         Field_Width := 1; -- Single character
      else
         Field_Width := Width; -- Input parameter
      end if;

      -- Verify correct data type to convert
      if Data.Class = Character_type then

         if Field_Width = 1 then
            In_The_String(Location) := Data.Character_Value;
         else
            if Left_justify then
               In_The_String(Location..Location + Field_Width - 1) :=
                 Data.Character_value & Blanks(1..Field_Width - 1);
            else
               In_The_String(Location..Location + Field_Width - 1) :=
                 Blanks(1..Field_width - 1) & Data.Character_Value;
            end if;
         end if;
         Location := Location + Field_Width;

      else -- Not correct data type to convert

         Format_Error(In_The_String, Location, Field_Width);

      end if;

   exception

      when others =>

         -- Use Default_Width global constant
         Format_Error(In_The_String, Location, Default_Width);

   end Format_character;
