generic

      -- User's instantiation enumeration type
      type Enumerated is (<>);

package Formatter is

   -- Purpose: Format variable numeric arguments
   --
   --  F        Returns the CONTENTS variant record set to the appropriate type
   --           to allow a common data type to be passed to either GET or PUT.
   --
   --  Put      Writes a formatted string given a variable number of data
   --           values and a FORMAT string.
   --
   --  Get      Returns a formatted string given a variable number of data
   --           values and a FORMAT string.

   type DP_Float is digits 15;    -- Double precision float type

   -- Allowable data types to format
   type Data_Type is (Integer_Type,
                      Float_Type,
                      DP_Float_Type,
                      String_Type,
                      Character_Type,
                      Unknown_Type);

   -- Abstract Data Type (ADT) specification
   type Contents(Class : Data_Type := Unknown_type) is private;

   -- Unconstrained array of data values to format
   type Values is array (positive range <>) of Contents;

   -- Specific data type to ADT conversion functions (overloaded)
   function F(Data : in      Integer) return Contents;
   function F(Data : in        Float) return Contents;
   function F(Data : in     DP_Float) return Contents;
   function F(Data : in    Character) return Contents;
   function F(Data : in       String) return Contents;
   function F(Data : in   Enumerated) return Contents;

   -- Output formatted values procedures (overloaded)
   procedure Put(Format : In String;
                 Value  : In Values);                        -- Multiple data values
   function SPut(Format : in String;
                 Value  : in Values) return String;
   procedure Put(Format : In String;
                 Value  : In Contents);                        -- Single data value
   procedure Put(Format : In String);                        -- No data values

   -- Output formatted values string functions (overloaded)
   function  Get(Format : In String;
                 Value  : In Values)     Return String; -- Multiple data values
   function  Get(Format : In String;
                 Value  : In Contents)   Return String; -- Single data value
   function  Get(Format : In String)     Return String; -- No data values

private

   -- Private string type
   type String_Pointer is access String;
   type String_Record is
      record
         The_String : String_Pointer;
         The_Length : Natural := 0;
   end record;

   -- Abstract Data Type implementation
   type Contents(Class : Data_Type := Unknown_type) is
      record
         case Class is
            when Integer_type     => Integer_value     : Integer;
            when Float_type       => Float_value       : Float;
            when DP_Float_type    => DP_Float_value    : DP_float;
            when Character_Type   => Character_Value   : Character;
            when String_type      => String_value      : String_Record;
            when Unknown_type     => null;
         end case;
      end record;

end Formatter;
