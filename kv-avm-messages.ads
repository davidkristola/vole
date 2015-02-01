with Ada.Streams;
with Ada.Finalization;
with Interfaces;

with kv.avm.Actor_References;
with kv.avm.Actor_References.Sets;
with kv.avm.Registers;
with kv.avm.Tuples;

package kv.avm.Messages is

   use Interfaces;
   use kv.avm.Registers;
   use kv.avm.Tuples;

   type Message_Type is tagged private;

   procedure Initialize
      (Self         : in out Message_Type);
   procedure Adjust
      (Self         : in out Message_Type);
   procedure Finalize
      (Self         : in out Message_Type);
   procedure Initialize
      (Self         : in out Message_Type;
       Source       : in     kv.avm.Actor_References.Actor_Reference_Type;
       Reply_To     : in     kv.avm.Actor_References.Actor_Reference_Type;
       Destination  : in     kv.avm.Actor_References.Actor_Reference_Type;
       Message_Name : in     String;
       Data         : in     kv.avm.Tuples.Tuple_Type;
       Future       : in     Interfaces.Unsigned_32);

   function Get_Name(Self : Message_Type) return String;
   function Get_Source(Self : Message_Type) return kv.avm.Actor_References.Actor_Reference_Type;
   function Get_Reply_To(Self : Message_Type) return kv.avm.Actor_References.Actor_Reference_Type;
   function Get_Destination(Self : Message_Type) return kv.avm.Actor_References.Actor_Reference_Type;
   function Get_Data(Self : Message_Type) return kv.avm.Tuples.Tuple_Type;
   function Get_Future(Self : Message_Type) return Interfaces.Unsigned_32;
   function Image(Self : Message_Type) return String;
   function Debug(Self : Message_Type) return String;
   function Reachable(Self : Message_Type) return kv.avm.Actor_References.Sets.Set;

   function "="(L, R: Message_Type) return Boolean;

   procedure Message_Write(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : in Message_Type);
   for Message_Type'WRITE use Message_Write;
   procedure Message_Read(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : out Message_Type);
   for Message_Type'READ use Message_Read;

private

   type Reference_Counted_Message_Type;
   type Reference_Counted_Message_Access is access all Reference_Counted_Message_Type;

   type Message_Type is new Ada.Finalization.Controlled with
      record
         Ref : Reference_Counted_Message_Access;
      end record;

end kv.avm.Messages;
