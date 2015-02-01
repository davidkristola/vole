with Ada.Finalization;

generic
   type Data_Type is private;
   type Data_Access is access Data_Type;
package kv.Ref_Counting_Mixin is
   type Control_Type is
      record
         Count : Natural := 0;
         Data  : Data_Access;
      end record;
   type Control_Access is access Control_Type;
   type Ref_Type is new Ada.Finalization.Controlled with
      record
         Control : Control_Access;
      end record;
   overriding procedure Initialize(Self : in out Ref_Type);
   overriding procedure Adjust(Self : in out Ref_Type);
   overriding procedure Finalize(Self : in out Ref_Type);
   procedure Set(Self : in out Ref_Type; Data : in     Data_Access);
   function Get(Self : Ref_Type) return Data_Access;
end kv.Ref_Counting_Mixin;
