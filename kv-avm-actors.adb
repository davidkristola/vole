with kv.avm.Log; use kv.avm.Log;

package body kv.avm.Actors is

   package Actors is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type => String,
       Element_Type => Actor_Access);

   Actor_Map : Actors.Map;

   -----------------------------------------------------------------------------
   procedure Empty_Actor_Map is
   begin
      Actor_Map.Clear;
   end Empty_Actor_Map;


   -----------------------------------------------------------------------------
   function New_Actor
      (Name            : in     String;
       Constructor     : in     kv.avm.Instructions.Code_Access;
       Attribute_Count : in     Natural := 0;
       Fixed_Registers : in     kv.avm.Memories.Register_Array_Type;
       Parent          : in     Actor_Access := null) return Actor_Access is

      Actor_Pointer : Actor_Access;
      Name_Pointer : kv.avm.Registers.Constant_String_Access;

   begin
      --Put_Line("kv.avm.actor.New_Actor="&Name);
      Actor_Pointer := new Actor_Type;
      Name_Pointer := new String'(Name);
      Actor_Pointer.Initialize(Name_Pointer, Constructor, Attribute_Count, Parent, Fixed_Registers);
      Actor_Map.Insert(Name, Actor_Pointer);
      return Actor_Pointer;
   end New_Actor;

   -----------------------------------------------------------------------------
   procedure Initialize
      (Self            : in out Actor_Type;
       Name            : in     kv.avm.Registers.Constant_String_Access;
       Constructor     : in     kv.avm.Instructions.Code_Access;
       Attribute_Count : in     Natural := 0;
       Parent          : in     Actor_Access := null;
       Fixed_Registers : in     kv.avm.Memories.Register_Array_Type) is
   begin
      Self.Name := Name;
      Self.Attribute_Count := Attribute_Count;
      Self.Methods.Insert("CONSTRUCTOR", kv.avm.Methods.New_Method("CONSTRUCTOR", Constructor));
      Self.Parent := Parent;
      Self.Fixed := Fixed_Registers;
   end Initialize;

   -----------------------------------------------------------------------------
   procedure Add_Method
      (Self   : in out Actor_Type;
       Method : in     kv.avm.Methods.Method_Access) is
   begin
      Self.Methods.Insert(Method.Get_Name, Method);
   end Add_Method;

   -----------------------------------------------------------------------------
   procedure Add_Parent
      (Self   : in out Actor_Type;
       Parent : in     Actor_Access) is
   begin
      Self.Parent := Parent;
   end Add_Parent;


   -----------------------------------------------------------------------------
   function Get_Method
      (Self : Actor_Type;
       Name : String) return kv.avm.Methods.Method_Access is

      Location : Subroutines.Cursor;
      use Subroutines;

   begin
      Location := Self.Methods.Find(Name);
      if Location = Subroutines.No_Element then
         if Self.Parent = null then
            return null;
         else
            return Self.Parent.Get_Method(Name);
         end if;
      else
         return Subroutines.Element(Location);
      end if;
   end Get_Method;

   -----------------------------------------------------------------------------
   function Get_Constants
      (Self : Actor_Type) return kv.avm.Memories.Register_Array_Type is
   begin
      return Self.Fixed;
   end Get_Constants;

   -----------------------------------------------------------------------------
   function Get_Parent
      (Self : Actor_Type) return Actor_Access is
   begin
      return Self.Parent;
   end Get_Parent;

   -----------------------------------------------------------------------------
   function Get_Name
      (Self : Actor_Type) return String is
   begin
      return Self.Name.all;
   end Get_Name;

   -----------------------------------------------------------------------------
   function Get_Actor_By_Name(Name : String) return Actor_Access is
      Location : Actors.Cursor;
      use Actors;
   begin
      Location := Actor_Map.Find(Name);
      if Location = Actors.No_Element then
         return null;
      else
         return Actors.Element(Location);
      end if;
   end Get_Actor_By_Name;

   function Image(Self : Actor_Type) return String is
   begin
      return Self.Name.all;
   end Image;

end kv.avm.Actors;
