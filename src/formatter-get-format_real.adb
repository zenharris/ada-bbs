separate(Formatter.Get)

   procedure Format_Real(Data            : in     Contents;
                         In_The_String   : in out String;
                         Location        : in out Natural;
                         Width           : in     Natural := 0;
                         Precision       : in     Natural := 0;
                         Exponent        : in     Natural := 0;
                         Fill_With_Zeros : in     Boolean := False) is

      -- ++
      --
      -- FUNCTIONAL DESCRIPTION:
      --
      --         Formats real number according to specified parameters.
      --
      -- FORMAL PARAMETERS:
      --
      --         Data:
      --          The input real number in a variant record.
      --
      --      In_The_String:
      --          The output string where the formatted real number is placed.
      --
      --      Location:
      --          The position of the formatted real number in the output string.
      --
      --      Width:
      --          The output formatted real number field width.
      --
      --      Precision:
      --          The number of decimal positions.
      --
      --      Exponent:
      --          The number of exponent positions.
      --
      --      Fill_With_Zeros:
      --          Logical (Boolean) flag specifying the formatted real number is to be
      --            padded with leading zeros.
      --
      -- DESIGN:
      --
      --         Format the real number directly into the output string using Float or
      --        Double-Float IO Put procedure.
      --
      -- --

      -- Local variable(s)
      Field_Width : Natural;

   begin

      -- Determine output field width
      if Width > 0 then
         Field_Width := Width; -- Set to specified width
      else
         Field_Width := Get.Default_Width;
      end if;

      if Data.Class = Float_Type then -- Correct data type

         -- Convert to string
         FIO.Put(ITEM => Data.Float_Value,
                 AFT  => Precision,
                 EXP  => Exponent,
                 TO   => In_The_String(Location..Location + Field_Width - 1));

         if Left_Justify then
            In_The_String(Location..Location + Field_Width - 1) :=
              Get.Left_Justified(In_The_String(Location..Location + Field_Width-1));
         end if;

         if Fill_With_Zeros then
            In_The_String(Location..Location + Field_Width - 1) :=
              Get.Zero_Fill(In_The_String(Location..Location + Field_Width-1));
         end if;

         -- Update next output position
         Location := Location + Field_Width;

      elsif Data.Class = DP_Float_Type then -- Correct data type

         -- Format directly to output string
         DFIO.Put(ITEM => Data.DP_Float_Value,
                  AFT  => Precision,
                  EXP  => Exponent,
                  TO   => In_The_String(Location..Location + Field_Width - 1));

         if Left_Justify then
            In_The_String(Location..Location + Field_Width - 1) :=
              Get.Left_Justified(In_The_String(Location..Location + Field_Width-1));
         end if;

         if Fill_With_Zeros then
            In_The_String(Location..Location + Field_Width - 1) :=
              Get.Zero_Fill(In_The_String(Location..Location + Field_Width-1));
         end if;

         Location := Location + Field_Width;

      else -- Not correct data type to convert

         Format_Error(In_The_String, Location, Field_Width);

      end if;

   exception

      when others =>

         Format_Error(In_The_String, Location, Get.Default_Width);

   end Format_Real;
