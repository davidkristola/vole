package body kv.avm.Actor_References is

   use Interfaces;

   Local_Domain : Interfaces.Unsigned_32 := 0;

   ----------------------------------------------------------------------------
   procedure Set_Local_Domain(Domain : Interfaces.Unsigned_32) is
   begin
      Local_Domain := Domain;
   end Set_Local_Domain;

   ----------------------------------------------------------------------------
   function Get_Local_Domain return Interfaces.Unsigned_32 is
   begin
      return Local_Domain;
   end Get_Local_Domain;


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self   : in out Actor_Reference_Type;
       Key    : in     Interfaces.Unsigned_32;
       Domain : in     Interfaces.Unsigned_32 := Get_Local_Domain) is
   begin
      Self.Key := Key;
      Self.Domain := Domain;
   end Initialize;

   ----------------------------------------------------------------------------
   function Get_Key(Self : Actor_Reference_Type) return Interfaces.Unsigned_32 is
   begin
      return Self.Key;
   end Get_Key;

   ----------------------------------------------------------------------------
   function Get_Domain(Self : Actor_Reference_Type) return Interfaces.Unsigned_32 is
   begin
      return Self.Domain;
   end Get_Domain;

   ----------------------------------------------------------------------------
   function Is_Local(Self : Actor_Reference_Type) return Boolean is
   begin
      return Self.Domain = Local_Domain;
   end Is_Local;

   ----------------------------------------------------------------------------
   function Image(Self : Actor_Reference_Type) return String is
      Domain : constant String := Interfaces.Unsigned_32'IMAGE(Self.Domain);
      use Interfaces;
   begin
      if Self.Key = Null_Reference.Key and Self.Domain = Null_Reference.Domain then
         return " NULL_REFERENCE";
      else
         return Interfaces.Unsigned_32'IMAGE(Self.Key) & "@" & Domain(2..Domain'LAST);
      end if;
   end Image;

   ----------------------------------------------------------------------------
   function "="(L, R : Actor_Reference_Type) return Boolean is
   begin
      return L.Domain = R.Domain and then L.Key = R.Key;
   end "=";

   ----------------------------------------------------------------------------
   function "<"(L, R : Actor_Reference_Type) return Boolean is
   begin
      return L.Domain < R. Domain or else (L.Domain = R.Domain and L.Key < R.Key);
   end "<";

end kv.avm.Actor_References;
