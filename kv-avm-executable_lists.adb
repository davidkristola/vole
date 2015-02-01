with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

with kv.avm.Log; use kv.avm.Log;

package body kv.avm.Executable_Lists is

   use kv.avm.Control;
   use kv.avm.Executables;

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Executable_Array_Type, Executable_Array_Access);


   -----------------------------------------------------------------------------
   function Get_List(Self : Executable_Handle_Type) return kv.avm.Control.Status_Type is
   begin
      return Self.Status;
   end Get_List;

   -----------------------------------------------------------------------------
   function Get_Cursor(Self : Executable_Handle_Type) return Cursor_Type is
   begin
      return Self.Position;
   end Get_Cursor;

   -----------------------------------------------------------------------------
   function Get_Reference(Self : Executable_Handle_Type) return kv.avm.Actor_References.Actor_Reference_Type is
   begin
      return Self.Reference;
   end Get_Reference;

   -----------------------------------------------------------------------------
   function Get_Executable(Self : Executable_Handle_Type) return kv.avm.Executables.Executable_Access is
   begin
      return Self.Executable;
   end Get_Executable;




   -----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Executable_Holder_Type;
       Kind : in     kv.avm.Control.Status_Type) is
   begin
      Self.Count := 0;
      Self.Kind := Kind;
      Self.List := new Executable_Array_Type(1 .. 8);
   end Initialize;

   -----------------------------------------------------------------------------
   procedure Add
      (Self : in out Executable_Holder_Type;
       This : in     Executable_Handle_Access) is

      New_Executable_List : Executable_Array_Access;

   begin
      if Self.Count = Self.List.all'LAST then
         -- We need more space.
         Put_Line("Machine Instance Control: doubling     allocated space");
         New_Executable_List := new Executable_Array_Type(1 .. Self.Count * 2);
         for I in Self.List.all'RANGE loop
            New_Executable_List(I) := Self.List(I);
         end loop;
         Free(Self.List);
         Self.List := New_Executable_List;
      end if;
      Self.Count := Self.Count + 1;
      This.Status := Self.Kind;
      This.Position := Self.Count;
      Self.List(Self.Count) := This;
   end Add;


   -----------------------------------------------------------------------------
   procedure Add
      (Self : in out Executable_Holder_Type;
       This : in     kv.avm.Executables.Executable_Access;
       Ref  : in     kv.avm.Actor_References.Actor_Reference_Type) is

      New_Handle : Executable_Handle_Access;

   begin
      New_Handle := new Executable_Handle_Type;
      New_Handle.Executable := This;
      New_Handle.Reference  := Ref;
      New_Handle.Status     := Self.Kind;
      New_Handle.Position   := Self.Count;
      Self.Add(New_Handle);
   end Add;

   -----------------------------------------------------------------------------
   function Find(Self : Executable_Holder_Type; Executable : kv.avm.Executables.Executable_Access) return Cursor_Type is
   begin
      for Index in 1 .. Self.Count loop
         if Self.List(Index).Executable = Executable then
            return Cursor_Type(Index);
         end if;
      end loop;
      return 0;
   end Find;

   -----------------------------------------------------------------------------
   function Is_In(Self : Executable_Holder_Type; Executable : kv.avm.Executables.Executable_Access) return Boolean is
      Cursor : Cursor_Type;
   begin
      Cursor := Self.Find(Executable);
      return Cursor /= 0;
   end Is_In;

   -----------------------------------------------------------------------------
   function Get(Self : Executable_Holder_Type; Position : Cursor_Type) return kv.avm.Executables.Executable_Access is
   begin
      return Self.List(Position).Executable;
   end Get;

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Executable_Handle_Type, Executable_Handle_Access);


   -----------------------------------------------------------------------------
   procedure Delete -- deallocate the handle
      (Self : in out Executable_Holder_Type;
       This : in     Cursor_Type) is

      Delete_Me : Executable_Handle_Access;

   begin
      Delete_Me := Self.List(This);
      Self.Drop(This);
      Free(Delete_Me);
   end Delete;

   -----------------------------------------------------------------------------
   procedure Drop
      (Self : in out Executable_Holder_Type;
       This : in Cursor_Type) is
   begin
      Self.List(This).Position := 0; -- Flag as being dropped
      if This /= Self.Count then
         -- Swap This with the last executable do that the list can be decreased from the end
         Self.List(This) := Self.List(Self.Count);
         Self.List(This).Position := This;
      end if;
      -- Clear the last position (where the target is now located) and decrement count
      Self.List(Self.Count) := null;
      Self.Count := Self.Count - 1;
   end Drop;

   -----------------------------------------------------------------------------
   procedure Drop
      (Self : in out Executable_Holder_Type;
       This : in kv.avm.Executables.Executable_Access) is
      Cursor : Cursor_Type;
   begin
      Cursor := Self.Find(This);
      if Cursor /= 0 then
         Self.Drop(Cursor);
      end if;
   end Drop;

   -----------------------------------------------------------------------------
   procedure Acquire_From
      (Self  : in out Executable_Holder_Type;
       Place : in     Cursor_Type;
       From  : in out Executable_Holder_Type) is

      It : Executable_Handle_Access;

   begin
      It := From.Get_Handle(Place);
      From.Drop(Place);
      Self.Add(It);
   end Acquire_From;


   -----------------------------------------------------------------------------
   function Get_Handle
      (Self     : Executable_Holder_Type;
       Position : Cursor_Type) return Executable_Handle_Access is
   begin
      return Self.List(Position);
   end Get_Handle;

   -----------------------------------------------------------------------------
   function Get_Last
      (Self : Executable_Holder_Type) return Cursor_Type is
   begin
      return Self.Count;
   end Get_Last;


end kv.avm.Executable_Lists;
