with Ada.Tags.Generic_Dispatching_Constructor;

package body kv.avm.Transactions is

   use Ada.Tags;

   -- Bind the dispatcher to the constructor method.
   function Construct_Tranaction is new Ada.Tags.Generic_Dispatching_Constructor
      (T           => Transactions_Interface,
       Parameters  => Obligatory_Parameter_Type,
       Constructor => Constructor);

   ----------------------------------------------------------------------------
   function Input_Transaction
      (Stream : not null access Ada.Streams.Root_Stream_Type'CLASS) return Transactions_Interface'CLASS is

      Parameters : aliased Obligatory_Parameter_Type;
      Which_Transaction : Ada.Tags.Tag;

   begin
      Ada.Tags.Tag'READ(Stream, Which_Transaction);
      declare
         Answer : Transactions_Interface'CLASS := Construct_Tranaction(Which_Transaction, Parameters'ACCESS);
      begin
         Transactions_Interface'CLASS'READ(Stream, Answer);
         return Answer;
      end;
   end Input_Transaction;

   ----------------------------------------------------------------------------
   procedure Output_Transaction
      (Stream : not null access Ada.Streams.Root_Stream_Type'CLASS;
       Item   : in Transactions_Interface'CLASS) is
   begin
      Ada.Tags.Tag'WRITE(Stream, Item'TAG);
      Transactions_Interface'CLASS'WRITE(Stream, Item);
   end Output_Transaction;

   ----------------------------------------------------------------------------
   function New_Transaction_From_Stream
      (Stream : not null access Ada.Streams.Root_Stream_Type'CLASS) return Transactions_Access is

      Item : Transactions_Interface'CLASS := Input_Transaction(Stream);

   begin
      return Item.New_Copy;
   end New_Transaction_From_Stream;

end kv.avm.Transactions;
