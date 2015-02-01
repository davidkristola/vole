with Ada.Containers.Indefinite_Ordered_Maps;

with kv.avm.Capabilities;
with kv.avm.Control;
with kv.avm.Registers;
with kv.avm.Actor_References;

package kv.avm.Brokers is

   type Broker_Type is new kv.avm.Capabilities.Capability_Interface with private;

   overriding procedure Execute
      (Self    : in out Broker_Type;
       Machine : in out kv.avm.Control.Control_Interface'CLASS;
       Input   : in     kv.avm.Registers.Register_Type;
       Output  :    out kv.avm.Registers.Register_Type;
       Status  :    out kv.avm.Control.Status_Type);

   procedure Add
      (Self            : in out Broker_Type;
       Actor_Reference : in     kv.avm.Actor_References.Actor_Reference_Type;
       Name            : in     String);

   procedure Delete
      (Self : in out Broker_Type;
       Name : in     String);

   function Is_Available
      (Self : in     Broker_Type;
       Name : in     String) return Boolean;

   function Get
      (Self : in     Broker_Type;
       Name : in     String) return kv.avm.Actor_References.Actor_Reference_Type;

private

   use kv.avm.Actor_References;

   package Actor_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => kv.avm.Actor_References.Actor_Reference_Type);

   type Broker_Type is new kv.avm.Capabilities.Capability_Interface with
      record
         Registrar : Actor_Maps.Map;
      end record;

end kv.avm.Brokers;
