with Ada.Finalization;

with kv.avm.Registers;
with kv.avm.Control;

package kv.avm.Capabilities is

   type Capability_Interface is interface;
   type Capability_Access is access all Capability_Interface'CLASS;

   procedure Execute
      (Self    : in out Capability_Interface;
       Machine : in out kv.avm.Control.Control_Interface'CLASS;
       Input   : in     kv.avm.Registers.Register_Type;
       Output  :    out kv.avm.Registers.Register_Type;
       Status  :    out kv.avm.Control.Status_Type) is abstract;


   type Capabilities_Type is new Ada.Finalization.Controlled with private;

   overriding procedure Initialize (Self : in out Capabilities_Type);
   overriding procedure Adjust     (Self : in out Capabilities_Type);
   overriding procedure Finalize   (Self : in out Capabilities_Type);

   function Has(Self : Capabilities_Type; Key : String) return Boolean;
   function Lookup(Self : Capabilities_Type; Key : String) return Capability_Access;
   procedure Add
      (Self  : in out Capabilities_Type;
       Key   : in     String;
       Value : in     Capability_Access);
   procedure Execute
      (Self    : in     Capabilities_Type;
       Key     : in     String;
       Machine : in out kv.avm.Control.Control_Interface'CLASS;
       Input   : in     kv.avm.Registers.Register_Type;
       Output  :    out kv.avm.Registers.Register_Type;
       Status  :    out kv.avm.Control.Status_Type);

private

   type Capabilities_Reference_Counter_Type;
   type Capabilities_Reference_Counter_Access is access all Capabilities_Reference_Counter_Type;

   type Capabilities_Type is new Ada.Finalization.Controlled with
      record
         Ref : Capabilities_Reference_Counter_Access;
      end record;

end kv.avm.Capabilities;
