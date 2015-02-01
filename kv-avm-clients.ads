with Ada.Unchecked_Deallocation;
with Interfaces;

with kv.avm.Transactions;

package kv.avm.Clients is

   type Client_Interface is interface;
   type Client_Access is access all Client_Interface'CLASS;

   type Status_Type is (Uninitialized, Closed, Running, Transacting, Failed);
   subtype Open_Status_Type is Status_Type range Running .. Transacting;

   procedure Bind_Address
      (Self    : in out Client_Interface;
       Address : in     String) is abstract;

   procedure Bind_Port
      (Self : in out Client_Interface;
       Port : in     Positive) is abstract;

   procedure Open
      (Self : in out Client_Interface) is abstract;

   procedure Close
      (Self : in out Client_Interface) is abstract;

   function Get_Status(Self : Client_Interface) return Status_Type is abstract;
   function Is_Open(Self : Client_Interface) return Boolean is abstract;

   procedure Send_Transaction
      (Self        : in out Client_Interface;
       Transaction : in     kv.avm.Transactions.Transactions_Interface'CLASS) is abstract;

   procedure Conclude_Transaction
      (Self : in out Client_Interface) is abstract;

   function Is_Transaction_Pending(Self : Client_Interface) return Boolean is abstract;
   function Get_Transaction(Self : Client_Interface) return kv.avm.Transactions.Transactions_Access is abstract;

   function Get_Domain(Self : Client_Interface) return Interfaces.Unsigned_32 is abstract;


   procedure Free is new Ada.Unchecked_Deallocation(Client_Interface'CLASS, Client_Access);


   type Client_Factory is interface;
   type Factory_Access is access all Client_Factory'CLASS;
   procedure New_Client
      (Self   : in out Client_Factory;
       Client :    out Client_Access) is abstract;

end kv.avm.Clients;
