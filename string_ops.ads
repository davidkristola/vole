-- public domain
with Ada.Calendar;
with Ada.Strings.Maps;
with Interfaces;

package String_Ops is

   pragma Elaborate_Body;

   type String_Pointer_Type is access String;

   -- All the control characters plus space and non-breaking-space.
   --
   function Is_White_Space
      (Char : in     Character) return Boolean;

   -- The Stuff routines place the source into the target,
   -- padding with space or zeros.
   --
   procedure Stuff
      (This : in     String;
       Into :    out String);

   procedure Stuff_Right
      (This : in     String;
       Into :    out String);

   procedure Stuff_Number
      (Number : in     Integer;
       Into   :    out String);

   procedure Stuff_Hex
      (Number : in     Interfaces.Unsigned_32;
       Into   :    out String);

   -- Kills Ada comments from The_String
   --
   function Drop_Ada_Comments
      (The_String :        String) return String;
   function Drop_Vole_Comments
      (The_String :        String) return String;

   function Up_Case
      (The_String :        String) return String;

   function Down_Case
      (The_String :        String) return String;

   -- Discards blanks from the front and back end
   -- of The_String.
   --
   function Trim_Blanks
      (The_String :        String) return String;

   -- Many times the important string will be enclosed in
   -- quotation marks or brackets.  This trims one character
   -- off of each end.
   --
   function Trim_One_From_Both_Ends(The_String : String) return String;

   -- Discards non-printable characters from
   -- The_String.
   --
   function Filter
      (The_String :        String) return String;

   -- Return the first substring of Str (deliniated by white space).
   -- Note: Str is assumed to not start with white space.
   --
   function First
      (Str : in     String) return String;

   -- Return the second part of Str.
   --
   function Rest
      (Str : in     String) return String;

   -- Return the second substring of Str (deliniated by white space).
   --
   function Second
      (Str : in     String) return String;

   -- This returns the Rest of the Rest.
   --
   function Second_Rest
      (Str : in     String) return String;

   -- Return the third substring of Str (deliniated by white space).
   --
   function Third
      (Str : in     String) return String;

   -- Return the Nth substring of Str (deliniated by white space).
   --
   function Nth
      (Str : in     String;
       N   : in     Positive) return String;

   -- Return the Nth substring of Str (deliniated by Set).
   --
   function Nth
      (Str : in     String;
       N   : in     Positive;
       Set : in     Ada.Strings.Maps.Character_Set) return String;

   -- This is a caseless compare.
   --
   function Same(L, R : in String) return Boolean;

   -- This is a specialized "=" for strings.  For
   -- the length of the smaller of Check and Master,
   -- both strings must be the same.
   --
   function Conformance
      (Master : in     String;
       Check  : in     String) return Boolean;


   -- Returns a string in the form "16#0000_0000#"
   --
   function Integer_To_Hex_String
      (Value : in     Interfaces.Unsigned_32) return String;


   -- These return images without leading blanks.
   --
   function Str
      (Int :        Integer) return String;
   function Str
      (Int :        Interfaces.Unsigned_32) return String;

   -- This "image" routine will only resort to scientific notation
   -- if the number is too large or too small.
   --
   function Img
      (Flt :         Float) return String;

   -- This will take a number (floating point or integer) from the
   -- string and return a Float.
   --
   function Value
      (Str :         String) return Float;

   function Img
      (Int :         Integer) return String renames Str;
   function Value
      (Str :         String) return Integer;

   function Long_Date
     (Date : in     Ada.Calendar.Time := Ada.Calendar.Clock) return String;
   function Short_Date
     (Date : in     Ada.Calendar.Time := Ada.Calendar.Clock) return String;
   function Date_As_Filename
     (Date : in     Ada.Calendar.Time := Ada.Calendar.Clock) return String;

   function IP_Address
      (Addr :        Interfaces.Unsigned_32) return String;

   -----------------------------------------------------------------------------
   generic
      type Selection_Type is (<>); -- enumeration
   package Selection_Ops is
      -- Return true if and only if Str passes "Conformance"
      -- to the image of a member of Selection_Type.
      -- Str is up cased before conformance checking.
      --
      function String_Matches
         (Str : in     String) return Boolean;

      -- Return the number of "Conformance" matches.
      --
      function String_Matches
         (Str : in     String) return Natural;

      -- Returns the first member of Selection_Type to
      -- conform to Str, or the first member of
      -- Selection_Type.
      --
      function To_Selection
         (Str : in     String) return Selection_Type;

   end Selection_Ops;

end String_Ops;
