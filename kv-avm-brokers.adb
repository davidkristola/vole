with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with kv.avm.Log;
with kv.avm.Tuples;

package body kv.avm.Brokers is

   use kv.avm.Registers;
   use kv.avm.Control;
   use kv.avm.Log;
   use Actor_Maps;

   -----------------------------------------------------------------------------
   procedure Execute
      (Self    : in out Broker_Type;
       Machine : in out kv.avm.Control.Control_Interface'CLASS;
       Input   : in     kv.avm.Registers.Register_Type;
       Output  :    out kv.avm.Registers.Register_Type;
       Status  :    out kv.avm.Control.Status_Type) is

      Parameters : kv.avm.Tuples.Tuple_Type;
      Operation : kv.avm.Registers.Register_Type;
      Key : kv.avm.Registers.Register_Type;

   begin
      Parameters := Input.Folded_Tuple;
      Operation := Parameters.Peek(0).all;
      Key := Parameters.Peek(1).all;
      Put_Line("Broker.Execute called with operation " & Reg_Img(Operation) & ", key " & Reg_Img(Key));
      Status := Active;
      if Operation.The_String = "Get" then
         Output := (Format => Actor_Reference, Instance => Self.Get(+Key.The_String));
      elsif Operation.The_String = "Has" then
         Output := (Format => Bit_Or_Boolean, Bit => Self.Is_Available(+Key.The_String));
      elsif Operation.The_String = "Add" then
         Self.Add(Parameters.Peek(2).Instance, +Key.The_String);
         Output := Make_U(0);
      else
         Status := Error;
      end if;
   end Execute;

   -----------------------------------------------------------------------------
   procedure Add
      (Self            : in out Broker_Type;
       Actor_Reference : in     kv.avm.Actor_References.Actor_Reference_Type;
       Name            : in     String) is
   begin
      Self.Registrar.Insert(Name, Actor_Reference);
   end Add;

   -----------------------------------------------------------------------------
   procedure Delete
      (Self : in out Broker_Type;
       Name : in     String) is
   begin
      Self.Registrar.Delete(Name);
   end Delete;

   -----------------------------------------------------------------------------
   function Is_Available
      (Self : in     Broker_Type;
       Name : in     String) return Boolean is
   begin
      return Self.Registrar.Contains(Name);
   end Is_Available;

   -----------------------------------------------------------------------------
   function Get
      (Self : in     Broker_Type;
       Name : in     String) return kv.avm.Actor_References.Actor_Reference_Type is
   begin
      return Self.Registrar.Element(Name);
   end Get;

end kv.avm.Brokers;
