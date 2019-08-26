--------------------------

   with Text_IO;
   with Formatter;

procedure Formatter_Test is

-- ++
--
-- FUNCTIONAL DESCRIPTION:
--
--         This is a test driver program for the generic Formatter package.
--
-- FORMAL PARAMETERS:
--
--         None.
--
-- DESIGN:
--
--         This test driver contains a number of calls to the Formatter Put
--        procedure with a format string and data values. Each test is identified
--        by a test number and a description of the test.
--
-- EXCEPTIONS:
--
--         No exceptions are declared in this test driver, although any exception
--        raised by Formatter.Put are handled.
--
-- KEYWORDS:
--
--         Test Driver.
--
-- --
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

    Name              : String(1..6);
    Integer_Value     : Positive     := 66;
    Real_Value        : Float        := 3.1415927;
    Character_Value   : Character    := 'x';
    Enumeration_Value : Days_of_Week := Thursday;

begin -- Formatter_Test

    Test_1:
    begin
       Name := "Test_1";
       Ada_Format.Put("%s:\tDefault Formats\n", F(Name));
       Ada_Format.Put("Integer_Value     = '%i'\n",  F(Integer_Value));
       Ada_Format.Put("Real_Value        = '%f'\n",  F(Real_Value));
       Ada_Format.Put("Scientific_Value  = '%e'\n",  F(Real_Value));
       Ada_Format.Put("Character_Value   = '%c'\n",   F(Character_Value));
       Ada_Format.Put("Enumeration_Value = '%s'\n\n", F(Enumeration_Value));
    exception
      when others =>
         Text_IO.Put_Line("Test_1: Unknown exception raised.");
         Text_IO.New_Line;
    end Test_1;

    Test_2:
    begin
       Name := "Test_2";
       Ada_Format.Put("%s:\t" &
                      "Wide-Field Formats\n" &
                      "Integer_Value     = '%15i'\n" &
                      "Real_Value        = '%15f'\n" &
                      "Scientific_Value  = '%15e'\n" &
                      "Character_Value   = '%15c'\n" &
                      "Enumeration_Value = '%15s'\n\n",
                      (F(Name),
                       F(Integer_Value),
                       F(Real_Value),
                       F(Real_Value),
                       F(Character_Value),
                       F(Enumeration_Value)));
    exception
      when others =>
         Text_IO.Put_Line("Test_2: Unknown exception raised.");
         Text_IO.New_Line;
    end Test_2;

    Test_3:
    begin
       Name := "Test_3";
       Ada_Format.Put("%s:\t" &
                      "Wide-Field Left-Justified Formats\n" &
                      "Integer_Value     = '%-15i'\n" &
                      "Real_Value        = '%-15f'\n" &
                      "Scientific_Value  = '%-15e'\n" &
                      "Character_Value   = '%-15c'\n" &
                      "Enumeration_Value = '%-15s'\n\n",
                      (F(Name),
                       F(Integer_Value),
                       F(Real_Value),
                       F(Real_Value),
                       F(Character_Value),
                       F(Enumeration_Value)));
    exception
      when others =>
         Text_IO.Put_Line("Test_3: Unknown exception raised.");
         Text_IO.New_Line;
    end Test_3;

    Test_4:
    begin
       Name := "Test_4";
       Ada_Format.Put("%s:\tDefault Formats, Zero-Fill\n", F(Name));
       Ada_Format.Put("Integer_Value     = '%0i'\n",    F(Integer_Value));
       Ada_Format.Put("Real_Value        = '%0f'\n",    F(Real_Value));
       Ada_Format.Put("Scientific_Value  = '%0e'\n\n",  F(Real_Value));
    exception
      when others =>
         Text_IO.Put_Line("Test_4: Unknown exception raised.");
         Text_IO.New_Line;
    end Test_4;

    Test_5:
    begin
       Name := "Test_5";
       Ada_Format.Put("%s:\t" &
                      "Specified Field Width, Non-Decimal Bases\n" &
                      "Integer Value     = '%4i'\n" &
                      "Hexadecimal Value = '%4x'\n" &
                      "Octal Value       = '%4o'\n\n",
                      (F(Name),
                       F(Integer_Value),
                       F(Integer_Value),
                       F(Integer_Value)));
    exception
      when others =>
         Text_IO.Put_Line("Test_5: Unknown exception raised.");
         Text_IO.New_Line;
    end Test_5;

    Test_6:
    begin
       Name := "Test_6";
       Ada_Format.Put("%s:\t" &
                      "Precision Formats\n" &
                      "Integer_Value     = '%15.4i'\n" &
                      "Real_Value        = '%15.4f'\n" &
                      "Scientific_Value  = '%15.4e'\n" &
                      "String_Value      = '%15.6s'\n\n",
                      (F(Name),
                       F(Integer_Value),
                       F(Real_Value),
                       F(Real_Value),
                       F(Name)));
    exception
      when others =>
         Text_IO.Put_Line("Test_6: Unknown exception raised.");
         Text_IO.New_Line;
    end Test_6;

    Test_7:
    begin
       Name := "Test_7";
       Ada_Format.Put("%s:\t" &
                      "Incorrect Field Widths\n" &
                      "Integer_Value     = '%1i'\n" &
                      "Real_Value        = '%2.1f'\n" &
                      "Scientific_Value  = '%3.2e'\n" &
                      "String_Value      = '%4s'\n" &
                      "Unknown Format    = '%+02,7z'\n\n",
                      (F(Name),
                       F(Integer_Value),
                       F(Real_Value),
                       F(Real_Value),
                       F(Name),
                       F(25)));
    exception
      when others =>
         Text_IO.Put_Line("Test_7: Unknown exception raised.");
         Text_IO.New_Line;
    end Test_7;

end Formatter_Test;
-----------------------------
