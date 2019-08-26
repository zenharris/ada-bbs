separate(Formatter.Get)

procedure Format_string (Data          : in     Contents;
                         In_The_String : in out String;
                         Location      : in out Natural;
                         Width         : in     Natural := 0;
                         Precision     : in     Natural := 0;
                         Left_Justify  : in     Boolean := False) is

-- ++
--
-- FUNCTIONAL DESCRIPTION:
--
--         Formats data string according to input parameters.
--
-- FORMAL PARAMETERS:
--
--         Data:
--          Input data string contained in variant record.
--
--      In_The_String:
--          Formatted Output String
--
--      Location:
--          Position in output string to place formatted input data string.
--
--      Width:
--          Field width of formatted output.
--
--      Precision:
--          Number of characters of input string to place in formatted output
--            field.
--
--      Left_Justify:
--          Logical (Boolean) switch which specifies to left-justify output
--            formatted string.
--
-- DESIGN:
--
--         If input string is greater than specified output field width then place
--        justified sub-string in output field. Otherwise, place justified string
--        in output field.
--
-- --

   -- Local variables
   Blanks     : String(1..255) := (others => ' ');
   Data_Width : integer;

begin

   -- Check data type
   if Data.Class = String_type then -- Is correct type to convert

      if Width = 0 then

         -- Put entire string into output buffer
         In_the_string(Location..Location + Data.String_value.The_Length - 1) :=
            Data.String_value.The_String.All;
         Location := Location + Data.String_value.The_Length;

      else -- Non-zero field Width specified

         Data_Width := Data.String_value.The_Length;

         if Data_width > Width then -- Data string too long

            if Precision > 0 then -- Sub-string specified

               if Left_justify then

                  In_The_String(Location..Location + Width - 1) :=
                      Data.String_value.The_String(1..Precision) & Blanks(1..Width - Precision);
                  Location := Location + Width;

               else -- Right-justify

                  In_The_String(Location..Location + WIDTH - 1) :=
                     Blanks(1..Width - Precision) & Data.String_value.The_String(1..Precision);
                  Location := Location + WIDTH;

               end if;

            else -- Truncate string to fit in width of field

               if Left_Justify then -- Take left-most "width" characters
                  In_the_string (Location..Location + Width - 1) := Data.String_value.The_String(1..Width);
               else                 -- Take right-most "width" characters
                  In_the_string (Location..Location + Width - 1) := Data.String_value.The_String(Data_Width - Width + 1..Data_Width);
               end if;

               Location := Location + Width;

            end if; -- Long String

         else -- String < specified field Width

            If Precision > 0 Then -- Sub-String Specified

               If Left_justify Then

                  In_the_string(Location..Location + Width - 1) :=
                     Data.String_value.The_String(1..Precision) & Blanks(1..Width - Precision);
                  Location := Location + Width;

               Else -- Right-Justify

                  In_the_string(Location..Location + Width - 1) :=
                     Blanks(1..Width - Precision) & Data.String_value.The_String(1..Precision);
                  Location := Location + Width;

               end if;

            else -- No substring specified

               If Left_justify Then

                  In_the_string(Location..Location + Width - 1) :=
                     Data.String_value.The_String.All & Blanks(1..Width - Data_width);
                  Location := Location + Width;

               else -- Right justify

                  In_the_string(Location..Location + Width - 1) :=
                     Blanks(1..Width - Data_width) & Data.String_value.The_String.All;
                  Location := Location + Width;

               end if; -- Justify test

            end if; -- Substring specified

         end if; -- Field width test

      end if;

   else -- Wrong class type for format specifier

      -- Uses Global Default_Width constant
      Format_Error(In_The_String, Location, Default_Width);

   end if; -- Class test

exception

      When others =>

      -- Uses Global Default_Width constant
      Format_Error(In_The_String, Location, Default_Width);

end Format_string;
