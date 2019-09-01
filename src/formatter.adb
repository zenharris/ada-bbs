
-------------------------------------------------------------------------------

with Text_IO;

package body Formatter is

   -- Instantiated Input-Output packages
   package   IO is new Text_io.Integer_io(Integer);
   package  FIO is new Text_io.Float_io(Float);
   package DFIO is new Text_io.Float_io(Dp_float);

   -- Overloaded Data Type to Variant Record conversion functions
   function F (Data : in Integer) return Contents is
   begin
      return Contents'(Class         => Integer_Type,
                       Integer_Value => Data);
   exception
      when others =>
         return Contents'(Class => Unknown_Type);
   end F;

   function F (Data : in Enumerated) return Contents is
      Data_String : constant String := Enumerated'Image(Data);
   begin
      return Contents'(Class        => String_Type,
                       String_Value => (The_String => new String'(Data_String),
                                        The_Length => Data_String'Length));
   exception
      when others =>
         return Contents'(Class => Unknown_Type);
   end F;

   function F (Data : in Float) return Contents is
   begin
      return Contents'(Class       => Float_Type,
                       Float_Value => Data);
   exception
      when others =>
         return Contents'(Class => Unknown_Type);
   end F;

   function F (Data : in Dp_Float) return Contents is
   begin
      return Contents'(Class          => Dp_Float_Type,
                       Dp_Float_Value => Data);
   exception
      when others =>
         return Contents'(Class => Unknown_Type);
   end F;

   function F (Data : in String) return Contents is
   begin
      return Contents'(Class        => String_Type,
                       String_Value => (The_String => new String'(Data),
                                        The_Length => Data'Length));
   exception
      when others =>
         return Contents'(Class => Unknown_Type);
   end F;

   function F (Data : in Character) return Contents is
   begin
      return Contents'(Class           => Character_Type,
                       Character_Value => Data);
   exception
      when others =>
         return Contents'(Class => Unknown_Type);
   end F;
   -- Overloaded Print Formatted Value procedures
   procedure Put(Format : in String;
                 Value  : in Values) is
   begin
      -- Write formatted string returned by Formatter.Get
      Text_Io.Put(Get(Format, Value));
   end Put;

   procedure PutS(retStr : in out String;
                  Format : in String;
                 Value  : in Values) is
   begin
      -- Write formatted string returned by Formatter.Get
      retStr := Get(Format, Value);
   end PutS;

   function SPut(Format : in String;
                 Value  : in Values) return String is
   begin
      -- Write formatted string returned by Formatter.Get
      return Get(Format, Value);
   end SPut;
   function SPut(Format : in String;
                 Value  : in Contents) return String is

      Value_List : Values(1..1) := (1 => Value);

   begin
      return SPut(Format => Format,
          Value  => Value_List);
   end SPut;


   procedure Put(Format : in String) is
      Value_List : Values (1..0);
   begin
      Put(Format => Format,
          Value  => Value_List);
   end Put;

   procedure Put(Format : in String;
                 Value  : in Contents) is

      Value_List : Values(1..1) := (1 => Value);

   begin
      Put(Format => Format,
          Value  => Value_List);
   end Put;

   -- Overloaded Formatted Value String functions
   function Get(Format : in String;
                Value  : in Values) return String
      is separate;

   function Get(Format : in String) return String is

      Value_List : Values (1..0);

   begin
      return Get(Format => Format,
                 Value  => Value_List);
   end Get;

   function Get(Format : in String;
                Value  : in Contents) return String is

      Value_List : Values(1..1) := (1 => Value);

   begin
      return Get(Format => Format,
                 Value  => Value_List);
   end Get;

end Formatter;
   -- separate(FORMATTER)
