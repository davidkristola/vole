with Ada.Containers.Indefinite_Ordered_Maps;

with kv.avm.Instructions;
with kv.avm.Registers;
with kv.avm.Memories;
with kv.avm.Methods; use kv.avm.Methods;

package kv.avm.Actors is

   use kv.avm.Instructions;

   package Subroutines is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => kv.avm.Methods.Method_Access);

   type Actor_Type;
   type Actor_Access is access all Actor_Type;
   type Actor_Type is tagged
      record
         Parent          : Actor_Access;
         Name            : kv.avm.Registers.Constant_String_Access;
         Methods         : Subroutines.Map;
         Attribute_Count : Natural;
         Fixed           : kv.avm.Memories.Register_Array_Type;
      end record;

   function New_Actor
      (Name            : in     String;
       Constructor     : in     kv.avm.Instructions.Code_Access;
       Attribute_Count : in     Natural := 0;
       Fixed_Registers : in     kv.avm.Memories.Register_Array_Type;
       Parent          : in     Actor_Access := null) return Actor_Access;

   procedure Initialize
      (Self            : in out Actor_Type;
       Name            : in     kv.avm.Registers.Constant_String_Access;
       Constructor     : in     kv.avm.Instructions.Code_Access;
       Attribute_Count : in     Natural := 0;
       Parent          : in     Actor_Access := null;
       Fixed_Registers : in     kv.avm.Memories.Register_Array_Type);

   procedure Add_Method
      (Self   : in out Actor_Type;
       Method : in     kv.avm.Methods.Method_Access);

   procedure Add_Parent
      (Self   : in out Actor_Type;
       Parent : in     Actor_Access);

   function Get_Method
      (Self : Actor_Type;
       Name : String) return kv.avm.Methods.Method_Access;

   function Get_Constants
      (Self : Actor_Type) return kv.avm.Memories.Register_Array_Type;

   function Get_Parent
      (Self : Actor_Type) return Actor_Access;

   function Get_Name
      (Self : Actor_Type) return String;

   function Get_Actor_By_Name(Name : String) return Actor_Access;

   function Image(Self : Actor_Type) return String;

   procedure Empty_Actor_Map;

end kv.avm.Actors;
