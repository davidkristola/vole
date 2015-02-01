with Ada.Containers.Vectors;
with Interfaces;

with kv.Ref_Counting_Mixin;
with kv.avm.Ini;
with kv.avm.Clients;
with kv.avm.Servers;
with kv.avm.Control;

package kv.avm.Affiliates is

   type Affiliates_Type is tagged private;

   procedure Initialize
      (Self     : in out Affiliates_Type;
       Settings : in     kv.avm.Ini.Settings_Type;
       Server   : in     kv.avm.Servers.Server_Access;
       Factory  : in     kv.avm.Clients.Factory_Access;
       Machine  : in     kv.avm.Control.Control_Access);

   function Client_Count(Self : Affiliates_Type) return Natural;

   function Get_Client(Self : Affiliates_Type; Index : Positive) return kv.avm.Clients.Client_Access;

   function Get_Domain_Client(Self : Affiliates_Type; Domain : Interfaces.Unsigned_32) return kv.avm.Clients.Client_Access;

   procedure Open_Clients
      (Self : in out Affiliates_Type);

   -- Clients and the server are asynchronous entities and they periodically need to be polled
   -- to pass data back and forth.  They are not allowed to interrupt the main thread.
   --
   procedure Periodic_Processing
      (Self : in out Affiliates_Type);


private

   use kv.avm.Clients;

   package Client_Lists is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => kv.avm.Clients.Client_Access);

   type Affiliate_Data_Type is
      record
         Clients : Client_Lists.Vector;
         Server  : kv.avm.Servers.Server_Access;
         Machine : kv.avm.Control.Control_Access;
         Factory : kv.avm.Clients.Factory_Access;
      end record;
   type Affiliate_Access is access Affiliate_Data_Type;
   package Ref_Count is new kv.Ref_Counting_Mixin(Affiliate_Data_Type, Affiliate_Access);

   type Affiliates_Type is tagged
      record
         Ref : Ref_Count.Ref_Type;
      end record;

end kv.avm.Affiliates;

