with Ada.Unchecked_Deallocation;
with Ada.Streams;

with kv.avm.Control;

package kv.avm.Transactions is

   type Obligatory_Parameter_Type is null record;


   -- Responsibility: represent an abstract inter-VM transaction
   --
   type Transactions_Interface is interface;
   type Transactions_Access is access all Transactions_Interface'CLASS;

   procedure Initialize
      (Self : in out Transactions_Interface) is abstract;

   function Equals
      (Left  : Transactions_Interface;
       Right : Transactions_Interface'CLASS) return Boolean is abstract;

   function Constructor
      (Info : not null access Obligatory_Parameter_Type) return Transactions_Interface is abstract;

   function New_Copy(Self : Transactions_Interface) return Transactions_Access is abstract;

   procedure Execute
      (Self    : in out Transactions_Interface;
       Machine : in     kv.avm.Control.Control_Access) is null;


   function Input_Transaction
      (Stream : not null access Ada.Streams.Root_Stream_Type'CLASS) return Transactions_Interface'CLASS;

   procedure Output_Transaction
      (Stream : not null access Ada.Streams.Root_Stream_Type'CLASS;
       Item   : in Transactions_Interface'CLASS);

   for Transactions_Interface'CLASS'INPUT use Input_Transaction;
   for Transactions_Interface'CLASS'OUTPUT use Output_Transaction;


   function New_Transaction_From_Stream
      (Stream : not null access Ada.Streams.Root_Stream_Type'CLASS) return Transactions_Access;


   procedure Free is new Ada.Unchecked_Deallocation(Transactions_Interface'CLASS, Transactions_Access);

end kv.avm.Transactions;
