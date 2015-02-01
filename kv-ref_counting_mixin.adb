with Ada.Unchecked_Deallocation;

package body kv.Ref_Counting_Mixin is

   procedure Free is new Ada.Unchecked_Deallocation(Data_Type, Data_Access);
   procedure Free is new Ada.Unchecked_Deallocation(Control_Type, Control_Access);


   -----------------------------------------------------------------------------
   procedure Initialize(Self : in out Ref_Type) is
   begin
      Self.Control := new Control_Type;
      Self.Control.Data := new Data_Type;
      Self.Control.Count := 1;
   end Initialize;

   -----------------------------------------------------------------------------
   procedure Adjust(Self : in out Ref_Type) is
      Control : Control_Access := Self.Control;
   begin
      if Control /= null then
         Control.Count := Control.Count + 1;
      end if;
   end Adjust;

   -----------------------------------------------------------------------------
   procedure Finalize(Self : in out Ref_Type) is
      Control : Control_Access := Self.Control;
   begin
      Self.Control := null;
      if Control /= null then
         Control.Count := Control.Count - 1;
         if Control.Count = 0 then
            Free(Control.Data);
            Free(Control);
         end if;
      end if;
   end Finalize;

   -----------------------------------------------------------------------------
   procedure Set(Self : in out Ref_Type; Data : in     Data_Access) is
   begin
      Self.Control.Data := Data;
   end Set;

   -----------------------------------------------------------------------------
   function Get(Self : Ref_Type) return Data_Access is
   begin
      return Self.Control.Data;
   end Get;

end kv.Ref_Counting_Mixin;
