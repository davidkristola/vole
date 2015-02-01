
with kv.avm.Control;
with kv.avm.Executables;
with kv.avm.Actor_References;

package kv.avm.Executable_Lists is

   type Cursor_Type is new Natural;
   subtype Index_Type is Cursor_Type range 1 .. Cursor_Type'LAST;

   type Executable_Handle_Type is tagged private;
   type Executable_Handle_Access is access Executable_Handle_Type;

   function Get_List(Self : Executable_Handle_Type) return kv.avm.Control.Status_Type;
   function Get_Cursor(Self : Executable_Handle_Type) return Cursor_Type;
   function Get_Reference(Self : Executable_Handle_Type) return kv.avm.Actor_References.Actor_Reference_Type;
   function Get_Executable(Self : Executable_Handle_Type) return kv.avm.Executables.Executable_Access;




   type Executable_Holder_Type is tagged private;

   procedure Initialize
      (Self : in out Executable_Holder_Type;
       Kind : in     kv.avm.Control.Status_Type);

   procedure Add
      (Self : in out Executable_Holder_Type;
       This : in     kv.avm.Executables.Executable_Access;
       Ref  : in     kv.avm.Actor_References.Actor_Reference_Type);

   function Find(Self : Executable_Holder_Type; Executable : kv.avm.Executables.Executable_Access) return Cursor_Type;
   function Is_In(Self : Executable_Holder_Type; Executable : kv.avm.Executables.Executable_Access) return Boolean;
   function Get(Self : Executable_Holder_Type; Position : Cursor_Type) return kv.avm.Executables.Executable_Access;

   procedure Delete -- deallocate the handle
      (Self : in out Executable_Holder_Type;
       This : in     Cursor_Type);

   procedure Drop -- just remove the handle from the list
      (Self : in out Executable_Holder_Type;
       This : in     Cursor_Type);

   procedure Drop
      (Self : in out Executable_Holder_Type;
       This : in     kv.avm.Executables.Executable_Access);

   procedure Acquire_From
      (Self  : in out Executable_Holder_Type;
       Place : in     Cursor_Type;
       From  : in out Executable_Holder_Type);

   function Get_Handle
      (Self     : Executable_Holder_Type;
       Position : Cursor_Type) return Executable_Handle_Access;

   function Get_Last
      (Self : Executable_Holder_Type) return Cursor_Type;

private

   type Executable_Handle_Type is tagged
      record
         Executable : kv.avm.Executables.Executable_Access;
         Reference  : kv.avm.Actor_References.Actor_Reference_Type;
         Status     : kv.avm.Control.Status_Type; -- This is also the list that holds the executable
         Position   : Cursor_Type; -- Zero means that this executable isn't in a list
      end record;

   type Executable_Array_Type is array (Index_Type range <>) of Executable_Handle_Access;
   type Executable_Array_Access is access Executable_Array_Type;

   type Executable_Holder_Type is tagged
      record
         List  : Executable_Array_Access;
         Count : Cursor_Type;
         Kind  : kv.avm.Control.Status_Type;
      end record;

   procedure Add
      (Self : in out Executable_Holder_Type;
       This : in     Executable_Handle_Access);

end kv.avm.Executable_Lists;
