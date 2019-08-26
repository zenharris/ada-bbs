  separate(Formatter.Get)


procedure Format_Number(Data            : in     Contents;
                        In_The_String   : in out String;
                        Location        : in out Natural;
                        Number_Base     : in     Natural := 10;
                        Width           : in     Natural :=  0;
                        Left_Justify    : in     Boolean := False;
                         Fill_With_Zeros : in     Boolean := False) is

-- ++
--
-- FUNCTIONAL DESCRIPTION:
--
--         Formats non-real number based on input parameters.
--
-- FORMAL PARAMETERS:
--
--         Data:
--          The input data to format, contained in a variant record.
--
--      In_The_String:
--          The output string where the formatted number is placed.
--
--      Location:
--          The position where the formatted number is placed.
--
--      Number_Base:
--          Which base to convert the number to: 8, 10, or 16.
--
--      Width:
--          The width of the formatted number.
--
--      Left_Justify:
--          Logical (Boolean) switch which causes the formatted number to be
--            left justified in the output field.
--
--      Fill_With_Zeros:
--          Logical (Boolean) switch which causes the right-justified, formatted
--            number to be padded with leading zeros.
--
-- DESIGN:
--
--         Determine output field width.
--        Convert number to correct number base.
--        Strip off number base delimiters.
--        Convert number to string.
--        Call Format_String to format number string.
--
-- EXCEPTIONS:
--
--         A constraint error will generate a default field of all asterisks.
--
-- --

   -- Local variables
   Temp_String     : String(1..255) := (others => ' ');
   Field_Width     : Natural; -- Output field width
   Based_Width     : Natural; -- Width of based number
   Number_String   : Formatter.Contents;

   -- Local functions
   function Based_Value_Of(The_String : in     String) return String is

      -- PURPOSE: Returns based value string without base type or delimiters

      -- Local constant
      Delimiter : constant Character := ASCII.SHARP; -- '#'

      -- Character indices
      First     : Positive;   -- Position of first digit after '#' delimiter
      Last      : Positive;   -- Position of last digit before '#' delimiter

   begin -- Based_Value_Of

      -- Find first digit after initial Delimiter
      First := The_String'First;
      while The_String(First) /= Delimiter loop
         if First = The_String'Last then                 -- Decimal number
            return The_String;                           -- No Delimiter found
         else
            First := First + 1;                          -- Check next char
         end if;
      end loop;
      First := First + 1;                                 -- Skip Initial Delimiter

      --  Find last digit before final Delimiter
      Last := First;
      while The_String(Last) /= Delimiter loop
         Last := Last + 1;
      end loop;
      Last   := Last - 1;                                 -- Ignore Terminal Delimiter

      return The_String(First..Last);

   end Based_Value_Of;

begin -- Format_Number

   -- Determine width of output
   if Width = 0 then
      Field_Width := Get.Default_Width;
   else
      Field_Width := Width;
   end if;

   -- Check for correct data type to format
   if Data.Class = Integer_type then

      -- Adjust field width for based value delimiters
      if Number_Base = 10 then
         Based_Width := Field_Width;
      else
         Based_Width := Field_Width + 4;
      end if;

      -- Convert integer value to string in correct Number_Base
      Io.Put(Item => Data.Integer_value,
             To   => Temp_string(1..Based_width),
             Base => Number_base);

      if Left_Justify then
         Temp_String(1..Based_Width) :=
            Get.Left_Justified(Temp_String(1..Based_Width));
      end if;

      if Number_Base = Base_Ten then

         if Fill_With_Zeros then

            -- Generate zero-filled Number_String
            Number_String := Contents'(Class => String_Type,
                                       String_Value => (The_String => new
                                                        String'(Get.Zero_Fill(Temp_String(1..Based_Width))),
                                                        The_Length => Based_Width));

         else

            -- Generate Number_String
            Number_String := Contents'(Class        => String_Type,
                                       String_Value => (The_String => new
                                                        String'(Temp_String(1..Based_Width)),
                                                        The_Length => Based_Width));

         end if;

      else

         Non_Decimal_Base:
         declare

             -- Strip off Based value delimiters
             Based_Value_String : constant String := Based_Value_Of(Temp_String(1..Based_Width));

         begin

             if Fill_With_Zeros then

                -- Generate zero-filled Number_String
                Number_String := Contents'(Class        => String_Type,
                                           String_Value => (The_String => new String'(Get.Zero_Fill(Based_Value_String)),
                                                            The_Length => Based_Value_String'length));

             else

                -- Generate Number_String
                Number_String := Contents'(Class        => String_Type,
                                           String_Value => (The_String => new String'(Based_Value_String),
                                                            The_Length => Based_Value_String'length));

             end if;

         end Non_Decimal_Base;
      end if;

      -- Insert Number_String into Output String
      Format_String(Number_String,In_The_String,Location,Field_Width);

   else -- Incorrect data type

      -- Fill field with error symbols
      Format_Error(In_The_String, Location, Field_Width);

   end if;

exception

   when others =>

     Format_Error(In_The_String, Location, Field_Width);

end Format_Number;
