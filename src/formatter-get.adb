

separate (Formatter)
   function Get (Format : in String;
                 Value  : in Values ) return String is

      -- ++
      --
      -- FUNCTIONAL DESCRIPTION:
      --
      --      Returns a formatted string given a FORMAT string and a
      --      variable number of data values.
      --
      -- FORMAL PARAMETERS:
      --
      --         Format : The input format string.
      --
      --        Value  : an array of data values.
      --
      -- RETURN VALUE:
      --
      --      The formatted string.
      --
      -- DESIGN:
      --
      --         for all input characters loop
      --            if input character is an escape character ("\") then
      --               translate to appropriate ASCII representation
      --               insert in output string at current position
      --               update current position
      --            if input character is a data format character ("%") then
      --               test for left justification ("-")
      --               test for zero fill ("0")
      --               get field width
      --               if decimal point present, skip over it
      --               get optional precision width
      --               get format specifier
      --               if character format specifier ("c") then Format_Character
      --               if scientific format specifier ("e") then Format_Real
      --               if floating point format specifier ("f") then Format_Real
      --               if integer number format specifier ("i") then Format_Number
      --               if octal number format specifier ("o") then Format_Number
      --               if string format specifier ("s") then Format_String
      --               if hexadecimal number format specifier ("x") then Format_Number
      --               else Format_Error string
      --            else copy character
      --        end loop
      --
      --
      -- EXCEPTIONS:
      --
      --         End_of_Format_String is raised by internal Get_Character procedure
      --                             when there are no more characters in the input
      --                             format string. Remaining data values, if any,
      --                             are ignored and the function returns the
      --                             Formatted_String.
      --
      -- KEYWORDS:
      --
      --         Format
      --
      -- --

      -- Input dependent variables
      C                : Character;                        -- Format character being examined
      Item             : Natural range 0..Value'last := 0; -- Value index

      -- Format specifier flags
      Fill_With_Zeros  : Boolean := False;
      Left_Justify     : Boolean := False;
      Specified_Width  : Integer;
      Precision        : Integer;

      -- Formatted output string
      Formatted_String : String(1..255) := (others => ' ');
      Source_Position  : Natural := 0; -- Input Format string character position
      Target_Position  : Natural := 1; -- Formatted string character position

      -- Temporary data values
      Octal_Value       : Integer := 0;
      Hexadecimal_Value : Integer := 0;

      -- Exception declarations
      End_Of_Format_String  : Exception;

      -- Constant declarations
      Escape_Character : constant character := Ascii.Back_Slash; -- '\'
      Data_Format      : constant character := Ascii.Percent;    -- '%'
      Default_Width    : constant           :=  8;
      Base_Eight       : constant           :=  8;
      Base_Ten         : constant           := 10;
      Base_Sixteen     : constant           := 16;

      -- Local program units
      procedure Get_Character ( C : out Character ) is
      begin
         C := Character(Format(Source_Position + 1)); -- Convert Working_String_Type to Character
         Source_Position := Source_Position + 1; -- Global symbol reference
      exception
         when Constraint_Error =>
            raise End_Of_Format_String;
      end Get_Character;

      function Is_Digit (C : in Character ) return Boolean is
      begin
         return C in '0'..'9';
      end Is_Digit;

      function Zero_Fill (The_String : in String ) return String is

         -- Local constants
         Blank       : constant character := ' ';
         Zero        : constant character := '0';

         -- Working String
         Temp_String : String(1..The_String'Length) := The_String;

      begin
         for Next_Character in Temp_String'range loop
            exit when Temp_String(Next_Character) /= Blank;
            Temp_String(Next_Character) := Zero;
         end loop;
         return Temp_String;
      end Zero_Fill;

      function Left_Justified(Data : in String) return String is
         Blank : constant Character := ' ';
         First : Natural := Data'First;
      begin -- Left_Justified
         -- Starting at the left, find first non-blank character
         while Data(First) = Blank loop
            First := First + 1;
         end loop;
         return Data(First..Data'Last) & Data(Data'First..First-1);
      end Left_Justified;

      procedure Format_Error(In_The_String : in out String;
                             Location      : in out Positive;
                             Width         : in     Positive) is
         Stars : String(1..Width) := (others => '*');
      begin
         In_The_String(Location..Location + Width - 1) := Stars;
         Location := Location + Width;
      end Format_Error;

      -- Data format conversion procedures
      procedure Format_character (Data : in     Contents;
                                  In_The_String : in out String;
                                  Location      : in out Natural;
                                  Width         : in     Natural := 0;
                                  Left_Justify  : in     Boolean := False) is separate;
      procedure Format_string    (Data : in     Contents;
                                  In_The_String : in out String;
                                  Location      : in out Natural;
                                  Width         : in     Natural := 0;
                                  Precision     : in     Natural := 0;
                                  Left_Justify  : in     Boolean := False) is separate;
      procedure Format_Number    (Data : in     Contents;
                                  In_The_String   : in out String;
                                  Location        : in out Natural;
                                  Number_Base     : in     Natural := 10;
                                  Width           : in     Natural :=  0;
                                  Left_Justify    : in     Boolean := False;
                                  Fill_With_Zeros : in     Boolean := False) is separate;
      procedure Format_Real      (Data  : in     Contents;
                                  In_The_String   : in out String;
                                  Location        : in out Natural;
                                  Width           : in     Natural := 0;
                                  Precision       : in     Natural := 0;
                                  Exponent        : in     Natural := 0;
                                  Fill_With_Zeros : in     Boolean := False) is separate;

   begin

      For_All_Characters : loop

         Get_Character(C);
         case C is

         when Escape_Character =>
            -- a) a 3 digit octal number follows
            -- b) a control character follows
            --    \n is newline
            --    \t is tab
            --    \b is backspace
            --    \r is carriage return
            --    \f is form feed
            -- c) a literal character follows
            --    \\ is backslash character
            --    \% is percent character
            --    \ followed by any other character is output as is

            Get_Character(C); -- Skip over Escape character

            case C is

               when '0'.. '7'  => -- Convert octal string

                  Octal_value := (Character'pos(C) - Character'pos('0'));
                  for Next in 1..2 loop
                     Get_Character(C);
                     Octal_value := 8 * Octal_value + (Character'pos(C) - Character'pos('0'));
                  end loop;
                  Formatted_String(Target_Position) := Character'Val(Octal_value);
                  Target_Position := Target_Position + 1;

               when 'n' => -- "\n" is a new line

                  Formatted_String(Target_Position..Target_Position + 1) := Ascii.Cr & Ascii.Lf;
                  Target_Position := Target_Position + 2;

               when 't' => -- "\t" is a tab

                  Formatted_String(Target_Position) := Ascii.Ht;
                  Target_Position := Target_Position + 1;

               when 'b' => -- "\b" is a backspace

                  Formatted_String(Target_Position) := Ascii.Bs;
                  Target_Position := Target_Position + 1;

               when 'r' => -- "\r" is a carriage return

                  Formatted_String(Target_Position) := Ascii.Cr;
                  Target_Position := Target_Position + 1;

               when 'f' => -- "\f" is a form feed

                  Formatted_String(Target_Position) := Ascii.Ff;
                  Target_Position := Target_Position + 1;

               when '\' => -- "\\" is the '\' character

                  Formatted_String(Target_Position) := C;
                  Target_Position := Target_Position + 1;

               when '%' => -- "\%" is the '%' character

                  Formatted_String(Target_Position) := C;
                  Target_Position := Target_Position + 1;

               when others => -- Literal character

                  Formatted_String(Target_Position) := C;
                  Target_Position := Target_Position + 1;

            end case;

         when Data_Format =>
            -- Format in form %-0w.pS where:
            --      - specifies left justification
            --      0 specifies zero fill
            --      w specifies the total field width (decimal value)
            --      p specifies the precision width (decimal value)
            --      s is the format specifier:
            --        c - character
            --        e - scientific (exponent) format
            --        f - floating point
            --        i - integer format
            --        o - octal format
            --        s - string
            --        x - hexadecimal format

            Get_Character(C); -- Skip over current data format character

            -- Initialize data format flags and values
            Left_Justify    := False;
            Fill_With_Zeros := False;
            Specified_Width := 0;
            Precision       := 0;

            -- Set data format flags and values
            if C = '-' then -- Set Left-justify string
               Left_justify := true;
               Get_Character(C); -- Skip over current '-' character in FORMAT
            end if;

            if C = '0' then -- Set Zero fill string
               Fill_with_zeros := true;
               Get_Character(C); -- Skip over current '0' character in FORMAT
            end if;

            while Is_digit(C) loop -- Get field width
               Specified_Width := 10 * Specified_Width + (Character'pos(C) - Character'pos('0'));
               Get_Character(C);
            end loop;

            if C = '.' then -- Skip precision separator character
               Get_Character(C);
            end if;

            while Is_digit(C) loop -- Get field precision
               Precision := 10 * Precision + (Character'pos(C) - Character'pos('0'));
               Get_Character(C);
            end loop;
            -- Exits with data format specifier in C

            -- Process data item according to format specifier
            Item := Item + 1;  -- Get next data item to format

            case C is -- Convert specification character

               when 'c' | 'C' => Format_character(Data => Value(Item),
                                                  In_The_String => Formatted_String,
                                                  Location      => Target_Position,
                                                  Width         => Specified_Width,
                                                  Left_Justify  => Left_Justify);

               when 'e' | 'E' => Format_Real(Data => Value(Item), -- Scientific Notation
                                             In_The_String   => Formatted_String,
                                             Location        => Target_Position,
                                             Width           => Specified_Width,
                                             Precision       => Precision,
                                             Exponent        => 3,
                                             Fill_With_Zeros => Fill_With_Zeros);

               when 'f' | 'F' => Format_Real(Data => Value(Item), -- Floating Decimal
                                             In_The_String   => Formatted_String,
                                             Location        => Target_Position,
                                             Width           => Specified_Width,
                                             Precision       => Precision,
                                             Fill_With_Zeros => Fill_With_Zeros);

               when 'i' | 'I' => Format_Number(Data => Value(Item), -- Decimal Number
                                               In_The_String   => Formatted_String,
                                               Location        => Target_Position,
                                               Number_Base     => Base_Ten,
                                               Width           => Specified_Width,
                                               Left_Justify    => Left_Justify,
                                               Fill_With_Zeros => Fill_With_Zeros);

               when 'o' | 'O' => Format_Number(Data => Value(Item), -- Octal Number
                                               In_The_String   => Formatted_String,
                                               Location        => Target_Position,
                                               Number_Base     => Base_Eight,
                                               Width           => Specified_Width,
                                               Left_Justify    => Left_Justify,
                                               Fill_With_Zeros => Fill_With_Zeros);

               when 's' | 'S' => Format_string(Data => Value(Item),
                                               In_The_String => Formatted_String,
                                               Location      => Target_Position,
                                               Width         => Specified_Width,
                                               Precision     => Precision,
                                               Left_Justify  => Left_Justify);

               when 'x' | 'X' => Format_Number(Data => Value(Item), -- Hexadecimal Number
                                               In_The_String   => Formatted_String,
                                               Location        => Target_Position,
                                               Number_Base     => Base_Sixteen,
                                               Width           => Specified_Width,
                                               Left_Justify    => Left_Justify,
                                               Fill_With_Zeros => Fill_With_Zeros);

               when others    => Format_Error(In_The_String => Formatted_String,
                                              Location      => Target_Position,
                                              Width         => Default_Width);
            end case;

         when others => -- Copy character

            Formatted_String(Target_Position) := C;
            Target_Position := Target_Position + 1;

         end case;

      end loop For_All_Characters;

      return Formatted_String(1..Target_Position - 1);

   exception

      when End_of_format_string =>

         return Formatted_String(1..Target_Position - 1);

   end Get;
