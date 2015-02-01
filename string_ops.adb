-- public domain
with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Interfaces;

package body String_Ops is

   use type Interfaces.Unsigned_32;
   use type Ada.Strings.Maps.Character_Set;

   Hex_Conversion : constant array (Interfaces.Unsigned_32 range 0..15)
      of character :=
         (00 => '0',
          01 => '1',
          02 => '2',
          03 => '3',
          04 => '4',
          05 => '5',
          06 => '6',
          07 => '7',
          08 => '8',
          09 => '9',
          10 => 'A',
          11 => 'B',
          12 => 'C',
          13 => 'D',
          14 => 'E',
          15 => 'F');

   -- All the control characters plus space and non-breaking-space.
   --
   White_Space_Set : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set(Ada.Characters.Latin_1.Space & Ada.Characters.Latin_1.NBSP) or
      Ada.Strings.Maps.Constants.Control_Set;


   -----------------------------------------------------------------------------
   function Is_White_Space
      (Char : in     Character) return Boolean is
   begin
      return Ada.Strings.Maps.Is_In(Char, White_Space_Set);
   end Is_White_Space;


   -----------------------------------------------------------------------------
   procedure Stuff
      (This : in     String;
       Into :    out String) is

   begin
      if This'LENGTH >= Into'LENGTH then
         Into := This(This'FIRST .. This'FIRST + Into'LENGTH - 1);
      else
         Into(Into'FIRST .. Into'FIRST + This'LENGTH - 1) := This;
         Into(Into'FIRST + This'LENGTH .. Into'LAST) := (others => ' ');
      end if;
   end Stuff;

   -----------------------------------------------------------------------------
   procedure Stuff_Right
      (This : in     String;
       Into :    out String) is

   begin
      if This'LENGTH >= Into'LENGTH then
         Into := This(This'FIRST .. This'FIRST + Into'LENGTH - 1);
      else
         Into(Into'LAST - This'LENGTH + 1 .. Into'LAST) := This;
         Into(Into'FIRST .. Into'LAST - This'LENGTH)    := (others => ' ');
      end if;
   end Stuff_Right;

   
   -----------------------------------------------------------------------------
   procedure Stuff_Number
      (Number : in     Integer;
       Into   :    out String) is

      Digit : Integer;
      Work  : Integer := abs(Number);

   begin
      for Index in reverse Into'RANGE loop
         Digit := Work mod 10;
         Into(Index) := Character'VAL(Digit + 48);
         Work := Work / 10;
      end loop;
   end Stuff_Number;


   -----------------------------------------------------------------------------
   procedure Stuff_Hex
      (Number : in     Interfaces.Unsigned_32;
       Into   :    out String) is

      Digit : Interfaces.Unsigned_32;
      Work  : Interfaces.Unsigned_32 := Number;

   begin
      for Index in reverse Into'RANGE loop
         Digit := Work mod 16;
         Into(Index) := Hex_Conversion(Digit);
         Work := Work / 16;
      end loop;
   end Stuff_Hex;


   -----------------------------------------------------------------------------
   function Drop_Ada_Comments
      (The_String :        String) return String is

      Index      : Integer := The_String'FIRST;
      Dash_Count : Integer := 0;

   begin
      Comment_Loop:
         while Index <= The_String'LAST loop
            if The_String (Index) = '-' then
               Dash_Count := Dash_Count + 1;
            else
               Dash_Count := 0;
            end if;
            if Dash_Count = 2 then
               Index := Index - 2; -- reset it to before the "--"
               exit Comment_Loop;
            else
               Index := Index + 1;
            end if;
         end loop Comment_Loop;
      return The_String (The_String'FIRST .. Index - 1);
   end Drop_Ada_Comments;

   -----------------------------------------------------------------------------
   function Drop_Vole_Comments
      (The_String : String) return String is

      In_Q : Boolean := False;

   begin
      for I in The_String'RANGE loop
         if The_String(I) = '"' then
            In_Q := not In_Q;
         end if;
         if (not In_Q) and then The_String(I) = '#' then
            return The_String(The_String'FIRST .. I-1);
         end if;
      end loop;
      return The_String;
   end Drop_Vole_Comments;


   -----------------------------------------------------------------------------
   function Up_Case
      (The_String :        String) return String is

   begin
      return Ada.Strings.Fixed.Translate(The_String, Ada.Strings.Maps.Constants.Upper_Case_Map);
   end Up_Case;


   -----------------------------------------------------------------------------
   function Down_Case
      (The_String :        String) return String is

   begin
      return Ada.Strings.Fixed.Translate(The_String, Ada.Strings.Maps.Constants.Lower_Case_Map);
   end Down_Case;


   -----------------------------------------------------------------------------
   function Trim_Blanks
      (The_String :        String) return String is

      Starting : Integer := The_String'FIRST;
      Ending   : Integer := The_String'LAST;

   begin
      while Starting < Ending loop
         if Is_White_Space(The_String(Starting)) then
            Starting := Starting + 1;
         else
            exit;
         end if;
      end loop;
      while Ending >= Starting loop
         if Is_White_Space(The_String(Ending)) then
            Ending := Ending - 1;
         else
            exit;
         end if;
      end loop;
      return The_String(Starting .. Ending);
   end Trim_Blanks;

   ----------------------------------------------------------------------------
   function Trim_One_From_Both_Ends(The_String : String) return String is
      Index_First : constant Positive := The_String'FIRST + 1;
      Index_Last : constant Positive := The_String'LAST - 1;
   begin
      return The_String(Index_First..Index_Last);
   end Trim_One_From_Both_Ends;


   -----------------------------------------------------------------------------
   function Filter
      (The_String :        String) return String is

      New_String : String(1..The_String'LENGTH);

      J : Positive := New_String'FIRST;

   begin
      for I in The_String'RANGE loop
         if The_String(I) in Ada.Characters.Latin_1.Space .. Ada.Characters.Latin_1.Tilde then
            New_String(J) := The_String(I);
            J := J + 1;
         end if;
      end loop;
      return New_String(1..J-1);
   end Filter;


   -----------------------------------------------------------------------------
   -- Return the first substring of Str (deliniated by a space).
   --
   function First
      (Str : in     String) return String is
   begin
      for I in Str'RANGE loop
         if Is_White_Space(Str(I)) then
            return Str(Str'FIRST..I-1);
         end if;
      end loop;
      return Str; -- no blanks, return the whole string.
   end First;


   -----------------------------------------------------------------------------
   -- Return the second part of Str.
   --
   function Rest
      (Str : in     String) return String is

      B : Boolean := False; -- found a blank

   begin
      for I in Str'RANGE loop
         if Is_White_Space(Str(I)) then
            B := True;
         elsif B then
            return Str(I..Str'LAST);
         end if;
      end loop;
      return ""; -- there was no second part to Str
   end Rest;


   -----------------------------------------------------------------------------
   function Second
      (Str : in     String) return String is

      type Search_State_Type is (Start, Blank_1, Word_1, Blank_2, Word_2);

      S : Search_State_Type := Start;
      L : Natural := 0;

   begin
      for I in Str'RANGE loop
         case S is
            when Start   => if     Is_White_Space(Str(I)) then S := Blank_1; else S := Word_1; end if;
            when Blank_1 => if not Is_White_Space(Str(I)) then S := Word_1; end if;
            when Word_1  => if     Is_White_Space(Str(I)) then S := Blank_2; end if;
            when Blank_2 => if not Is_White_Space(Str(I)) then S := Word_2; L := I; end if;
            when Word_2  => if     Is_White_Space(Str(I)) then return Str(L..I-1); end if;
         end case;
      end loop;
      if S = Word_2 then
         return Str(L..Str'LAST);
      else
         return ""; -- there was no second part to Str
      end if;
   end Second;


   -----------------------------------------------------------------------------
   function Second_Rest
      (Str : in     String) return String is

      type Search_State_Type is (Start, Blank_1, Word_1, Blank_2, Word_2, Blank_3);

      S : Search_State_Type := Start;

   begin
      for I in Str'RANGE loop
         case S is
            when Start   => if     Is_White_Space(Str(I)) then S := Blank_1; else S := Word_1; end if;
            when Blank_1 => if not Is_White_Space(Str(I)) then S := Word_1; end if;
            when Word_1  => if     Is_White_Space(Str(I)) then S := Blank_2; end if;
            when Blank_2 => if not Is_White_Space(Str(I)) then S := Word_2; end if;
            when Word_2  => if     Is_White_Space(Str(I)) then S := Blank_3; end if;
            when Blank_3 => if not Is_White_Space(Str(I)) then return Str(I..Str'LAST); end if;
         end case;
      end loop;
      return ""; -- there was no third part to Str
   end Second_Rest;


   -----------------------------------------------------------------------------
   function Third
      (Str : in     String) return String is

      type Search_State_Type is (Start, Blank_1, Word_1, Blank_2, Word_2, Blank_3, Word_3);

      S : Search_State_Type := Start;
      L : Natural := 0;

   begin
      for I in Str'RANGE loop
         case S is
            when Start   => if     Is_White_Space(Str(I)) then S := Blank_1; else S := Word_1; end if;
            when Blank_1 => if not Is_White_Space(Str(I)) then S := Word_1; end if;
            when Word_1  => if     Is_White_Space(Str(I)) then S := Blank_2; end if;
            when Blank_2 => if not Is_White_Space(Str(I)) then S := Word_2; end if;
            when Word_2  => if     Is_White_Space(Str(I)) then S := Blank_3; end if;
            when Blank_3 => if not Is_White_Space(Str(I)) then S := Word_3; L := I; end if;
            when Word_3  => if     Is_White_Space(Str(I)) then return Str(L..I-1); end if;
         end case;
      end loop;
      if S = Word_3 then
         return Str(L..Str'LAST);
      else
         return ""; -- there was no second part to Str
      end if;
   end Third;


   -----------------------------------------------------------------------------
   function Nth
      (Str : in     String;
       N   : in     Positive) return String is

      Init_State : constant := 0;
      State      : Natural  := Init_State; -- 0 = init, odd = blank, even = word
      Start      : Natural  := Str'FIRST;
      Last_Word  : Positive := N * 2;
      Last_Blank : Positive := Last_Word - 1;

   begin
      for I in Str'RANGE loop
         if    State = Init_State then if     Is_White_Space(Str(I)) then State := 1; else State := 2;    end if;
         elsif State = Last_Blank then if not Is_White_Space(Str(I)) then State := State + 1; Start := I; end if;
         elsif State = Last_Word  then if     Is_White_Space(Str(I)) then return Str(Start..I - 1);       end if;
         elsif State mod 2 = 1    then if not Is_White_Space(Str(I)) then State := State + 1;             end if;
         else                          if     Is_White_Space(Str(I)) then State := State + 1;             end if;
         end if;
      end loop;
      if State = Last_Word then
         return Str(Start..Str'LAST);
      else
         return ""; -- there was no second part to Str
      end if;
   end Nth;


   -----------------------------------------------------------------------------
   function Nth
      (Str : in     String;
       N   : in     Positive;
       Set : in     Ada.Strings.Maps.Character_Set) return String is

      Init_State : constant := 0;
      State      : Natural  := Init_State; -- 0 = init, odd = blank, even = word
      Start      : Natural  := Str'FIRST;
      Last_Word  : Positive := N * 2;
      Last_Blank : Positive := Last_Word - 1;

      use Ada.Strings.Maps;

   begin
      for I in Str'RANGE loop
         if    State = Init_State then if     Is_In(Str(I), Set) then State := 1; else State := 2;    end if;
         elsif State = Last_Blank then if not Is_In(Str(I), Set) then State := State + 1; Start := I; end if;
         elsif State = Last_Word  then if     Is_In(Str(I), Set) then return Str(Start..I - 1);       end if;
         elsif State mod 2 = 1    then if not Is_In(Str(I), Set) then State := State + 1;             end if;
         else                          if     Is_In(Str(I), Set) then State := State + 1;             end if;
         end if;
      end loop;
      if State = Last_Word then
         return Str(Start..Str'LAST);
      else
         return ""; -- there was no second part to Str
      end if;
   end Nth;


   -----------------------------------------------------------------------------
   function Same(L, R : in String) return Boolean is
      A : String(1..L'LENGTH);
      B : String(1..R'LENGTH);
   begin
      if L'LENGTH = R'LENGTH then
         A := Ada.Strings.Fixed.Translate(L, Ada.Strings.Maps.Constants.Lower_Case_Map);
         B := Ada.Strings.Fixed.Translate(R, Ada.Strings.Maps.Constants.Lower_Case_Map);
         return A = B;
      end if;
      return False;
   end Same;


   -----------------------------------------------------------------------------
   function Conformance
      (Master : in     String;
       Check  : in     String) return Boolean is

      Master_I  : Positive := Master'FIRST;
      Check_I   : Positive := Check'FIRST;
      Different : Boolean  := False;

   begin
      --if Verbose then Put_Line("Conformance master <"&Master&"> = <"&Check&">?"); end if;
      while Master_I <= Master'LAST and Check_I <= Check'LAST and not Different loop
         Different := Master(Master_I) /= Check(Check_I);
         Master_I := Master_I + 1;
         Check_I  := Check_I  + 1;
      end loop;
      return not Different;
   end Conformance;


   -----------------------------------------------------------------------------
   function Integer_To_Hex_String
      (Value : in     Interfaces.Unsigned_32) return String is

      Hex_String_Value : String (1 .. 13) := "16#0000_0000#";
      Remainder : Interfaces.Unsigned_32 := Value;
      Hex_Value : Interfaces.Unsigned_32;
      Position : Integer := 12;

   begin
      while Remainder /= 0 and Position > 3 loop

         Hex_Value := Remainder mod 16;
         Remainder := (Remainder - Hex_Value) / 16;

         Hex_String_Value(Position) := Hex_Conversion(Hex_Value);

         Position := Position - 1;
         if Position = 8 then
            Position := 7;
         end if;

      end loop;

      return Hex_String_Value;
   end Integer_To_Hex_String;


   -----------------------------------------------------------------------------
   function Str
      (Int :        Integer) return String is

      Last : constant := 30;
      Zero : constant := Character'Pos('0');

      Converted_Number : String(1..Last);
      Number           : Integer := abs(Int);
      Digit            : Integer := 0;
      Index            : Integer := Last + 1;

   begin
      if Int = 0 then
         Index := Index - 1;
         Converted_Number(Index) := '0';
      else
         while Number /= 0 loop
            Digit := Number rem 10;
            Number := Number / 10;
            Index := Index - 1;
            Converted_Number(Index) := Character'Val(Zero + Digit);
         end loop;
         if Int < 0 then
            Index := Index - 1;
            Converted_Number(Index) := '-';
         end if;
      end if;
      return Converted_Number(Index..Last);
   end Str;


   -----------------------------------------------------------------------------
   function Str
      (Int :        Interfaces.Unsigned_32) return String is

      Last : constant := 30;

      Converted_Number : String(1..Last);
      Number           : Interfaces.Unsigned_32 := Int;
      Digit            : Interfaces.Unsigned_32 := 0;
      Index            : Integer                := Last + 1;

   begin
      if Int = 0 then
         Index := Index - 1;
         Converted_Number(Index) := '0';
      else
         while Number /= 0 loop
            Digit := Number rem 16;
            Number := Number / 16;
            Index := Index - 1;
            Converted_Number(Index) := Hex_Conversion(Digit);
         end loop;
      end if;
      return Converted_Number(Index..Last);
   end Str;


   -----------------------------------------------------------------------------
   function Img
      (Flt :         Float) return String is

      package Flt_IO is new Ada.Text_IO.Float_IO(Float);

      A   : constant Float := abs(Flt);
      F   : String(1..20)  := (others => ' ');
      Aft : Natural;

   begin
      if A in 0.001 .. 10000.0 then
         if A >= 10000.0 then
            Aft := 1;
         elsif A >= 1000.0 then
            Aft := 2;
         elsif A >= 100.0 then
            Aft := 3;
         elsif A >= 10.0 then
            Aft := 4;
         elsif A >= 1.0 then
            Aft := 5;
         elsif A >= 0.1 then
            Aft := 6;
         elsif A >= 0.01 then
            Aft := 7;
         else
            Aft := 8;
         end if;
         Flt_IO.Put(F, Flt, Aft, Exp => 0);
         return Trim_Blanks(F);
      else
         return Trim_Blanks(Float'IMAGE(Flt));
      end if;
   end Img;


   -----------------------------------------------------------------------------
   function Value
      (Str :         String) return Float is

      Has_Dot : Boolean := False;

   begin
      for I in Str'RANGE loop
         Has_Dot := Str(I) = '.';
         exit when Has_Dot;
      end loop;
      if Has_Dot then
         return Float'VALUE(Str);
      else
         return Float(Integer'VALUE(Str));
      end if;
   end Value;


   -----------------------------------------------------------------------------
   function Value
      (Str :         String) return Integer is

   begin
      return Integer'VALUE(Str);
   end Value;


   -----------------------------------------------------------------------------
   function IP_Address
      (Addr :        Interfaces.Unsigned_32) return String is

      Temp : Interfaces.Unsigned_32 := Addr;
      A, B, C, D : Integer;

   begin
      D := Integer(Temp mod 16#1_00#);
      Temp := Temp / 16#1_00#;
      C := Integer(Temp mod 16#1_00#);
      Temp := Temp / 16#1_00#;
      B := Integer(Temp mod 16#1_00#);
      A := Integer(Temp / 16#1_00#);
      return Str(A) & '.' & Str(B) & '.' & Str(C) & '.' & Str(D);
   end IP_Address;


   type Month_Name_Table_Type is array (Ada.Calendar.Month_Number) of String_Pointer_Type;

   Month_Name_Table : constant Month_Name_Table_Type :=
      (01 => new String'("January"),
       02 => new String'("February"),
       03 => new String'("March"),
       04 => new String'("April"),
       05 => new String'("May"),
       06 => new String'("June"),
       07 => new String'("July"),
       08 => new String'("August"),
       09 => new String'("September"),
       10 => new String'("October"),
       11 => new String'("November"),
       12 => new String'("December"));


   -----------------------------------------------------------------------------
   function Long_Date
     (Date : in     Ada.Calendar.Time := Ada.Calendar.Clock) return String is

      Year          : Ada.Calendar.Year_Number;
      Month         : Ada.Calendar.Month_Number;
      Day           : Ada.Calendar.Day_Number;
      Seconds       : Ada.Calendar.Day_Duration;
      Hours         : Integer;
      Minutes       : Integer;
      Secs          : Integer;
      Hours_Image   : String(1..2);
      Minutes_Image : String(1..2);
      Seconds_Image : String(1..2);

      AM_PM : String := "am";
      
      C : constant Character := ':';

   begin
      Ada.Calendar.Split
        (Date    => Date,
         Year    => Year,
         Month   => Month,
         Day     => Day,
         Seconds => Seconds);

      Secs    := Integer(Seconds);
      Minutes := Secs / 60;
      Secs    := Secs mod 60;
      Hours   := Minutes / 60;
      Minutes := Minutes mod 60;

      if Hours > 12 then
         AM_PM := "pm";
         Hours := Hours - 12;
      end if;

      Stuff_Number(Hours, Hours_Image);
      Stuff_Number(Minutes, Minutes_Image);
      Stuff_Number(Secs, Seconds_Image);

      return Month_Name_Table(Month).all & Ada.Calendar.Day_Number'IMAGE(Day) &
         ", " & Str(Year) & ", " & Hours_Image &C& Minutes_Image &C& Seconds_Image &
         " " & AM_PM;
   end Long_Date;


   -----------------------------------------------------------------------------
   function Short_Date
     (Date : in     Ada.Calendar.Time := Ada.Calendar.Clock) return String is

      Year          : Ada.Calendar.Year_Number;
      Month         : Ada.Calendar.Month_Number;
      Day           : Ada.Calendar.Day_Number;
      Seconds       : Ada.Calendar.Day_Duration;
      Hours         : Integer;
      Minutes       : Integer;
      Secs          : Integer;
      Month_Image   : String(1..2);
      Day_Image     : String(1..2);
      Hours_Image   : String(1..2);
      Minutes_Image : String(1..2);
      Seconds_Image : String(1..2);

      C : constant Character := ':';
      S : constant Character := '/';

   begin
      Ada.Calendar.Split
        (Date    => Date,
         Year    => Year,
         Month   => Month,
         Day     => Day,
         Seconds => Seconds);

      Secs    := Integer(Seconds);
      Minutes := Secs / 60;
      Secs    := Secs mod 60;
      Hours   := Minutes / 60;
      Minutes := Minutes mod 60;

      Stuff_Number(Month, Month_Image);
      Stuff_Number(Day, Day_Image);
      Stuff_Number(Hours, Hours_Image);
      Stuff_Number(Minutes, Minutes_Image);
      Stuff_Number(Secs, Seconds_Image);

      return Str(Year) &S& Month_Image &S& Day_Image & ' ' & Hours_Image &C& Minutes_Image &C& Seconds_Image;
   end Short_Date;


   -----------------------------------------------------------------------------
   function Date_As_Filename
     (Date : in     Ada.Calendar.Time := Ada.Calendar.Clock) return String is

      Year          : Ada.Calendar.Year_Number;
      Month         : Ada.Calendar.Month_Number;
      Day           : Ada.Calendar.Day_Number;
      Seconds       : Ada.Calendar.Day_Duration;
      Hours         : Integer;
      Minutes       : Integer;
      Secs          : Integer;
      Month_Image   : String(1..2);
      Day_Image     : String(1..2);
      Hours_Image   : String(1..2);
      Minutes_Image : String(1..2);
      Seconds_Image : String(1..2);

   begin
      Ada.Calendar.Split
        (Date    => Date,
         Year    => Year,
         Month   => Month,
         Day     => Day,
         Seconds => Seconds);

      Secs    := Integer(Seconds);
      Minutes := Secs / 60;
      Secs    := Secs mod 60;
      Hours   := Minutes / 60;
      Minutes := Minutes mod 60;

      Stuff_Number(Month, Month_Image);
      Stuff_Number(Day, Day_Image);
      Stuff_Number(Hours, Hours_Image);
      Stuff_Number(Minutes, Minutes_Image);
      Stuff_Number(Secs, Seconds_Image);

      return 'y' & Str(Year) & 'm' & Month_Image & 'd' & Day_Image & "_h" &
         Hours_Image & 'm' & Minutes_Image & 's' & Seconds_Image;
   end Date_As_Filename;


   -----------------------------------------------------------------------------
   package body Selection_Ops is

      -----------------------------------------------------------------------------
      function String_Matches
         (Str : in     String) return Boolean is

         Match_Count : Natural         := 0;
         Cap_Str     : constant String := Up_Case(Str);

      begin
         for Index in Selection_Type loop
            if Conformance(Selection_Type'IMAGE(Index), Cap_Str) then
               Match_Count := Match_Count + 1;
            end if;
         end loop;
         return Match_Count = 1;
      end String_Matches;


      -----------------------------------------------------------------------------
      -- Return the number of "Conformance" matches.
      --
      function String_Matches
         (Str : in     String) return Natural is

         Match_Count : Natural         := 0;
         Cap_Str     : constant String := Up_Case(Str);

      begin
         for Index in Selection_Type loop
            if Conformance(Selection_Type'IMAGE(Index), Cap_Str) then
               Match_Count := Match_Count + 1;
            end if;
         end loop;
         return Match_Count;
      end String_Matches;


      -----------------------------------------------------------------------------
      function To_Selection
         (Str : in     String) return Selection_Type is
         Cap_Str : constant String := Up_Case(Str);
      begin
         for Index in Selection_Type loop
            if Conformance(Selection_Type'IMAGE(Index), Cap_Str) then
               return Index;
            end if;
         end loop;
         return Selection_Type'FIRST; -- error state
      end To_Selection;

   end Selection_Ops;


end String_Ops;
