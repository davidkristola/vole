with Ada.Unchecked_Deallocation;

package body kv.avm.Messages is

   type Message_Data_Type is
      record
         Source_Actor      : kv.avm.Actor_References.Actor_Reference_Type;
         Reply_To          : kv.avm.Actor_References.Actor_Reference_Type;
         Destination_Actor : kv.avm.Actor_References.Actor_Reference_Type;
         Message_Name      : kv.avm.Registers.String_Type;
         Data              : kv.avm.Tuples.Tuple_Type;
         Future            : Interfaces.Unsigned_32;
      end record;
   type Message_Data_Access is access Message_Data_Type;

   type Reference_Counted_Message_Type is
      record
         Count : Natural := 0;
         Data  : Message_Data_Access;
      end record;

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Reference_Counted_Message_Type, Reference_Counted_Message_Access);

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Message_Data_Type, Message_Data_Access);


   -----------------------------------------------------------------------------
   function Debug(Data : Message_Data_Access) return String is
   begin
      return "N: '" & (+Data.Message_Name) & "'" &
            " S:" & Data.Source_Actor.Image &
            " R:" & Data.Reply_To.Image &
            " D:" & Data.Destination_Actor.Image &
            " T: " & Data.Data.To_String &
            " F:" & Interfaces.Unsigned_32'IMAGE(Data.Future);
   end Debug;

   -----------------------------------------------------------------------------
   procedure Initialize
      (Self         : in out Message_Type) is
   begin
      null;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Adjust
      (Self         : in out Message_Type) is
   begin
      if Self.Ref /= null then
         Self.Ref.Count := Self.Ref.Count + 1;
      end if;
   end Adjust;

   ----------------------------------------------------------------------------
   procedure Finalize
      (Self         : in out Message_Type) is
      Ref : Reference_Counted_Message_Access := Self.Ref;
   begin
      Self.Ref := null;
      if Ref /= null then
         Ref.Count := Ref.Count - 1;
         if Ref.Count = 0 then
            Free(Ref.Data);
            Free(Ref);
         end if;
      end if;
   end Finalize;

   -----------------------------------------------------------------------------
   procedure Initialize
      (Self         : in out Message_Type;
       Source       : in     kv.avm.Actor_References.Actor_Reference_Type;
       Reply_To     : in     kv.avm.Actor_References.Actor_Reference_Type;
       Destination  : in     kv.avm.Actor_References.Actor_Reference_Type;
       Message_Name : in     String;
       Data         : in     kv.avm.Tuples.Tuple_Type;
       Future       : in     Interfaces.Unsigned_32) is
      use kv.avm.Registers;
   begin
      Self.Ref := new Reference_Counted_Message_Type;
      Self.Ref.Count := 1;
      Self.Ref.Data := new Message_Data_Type;

      Self.Ref.Data.Source_Actor := Source;
      Self.Ref.Data.Reply_To := Reply_To;
      Self.Ref.Data.Destination_Actor := Destination;
      Self.Ref.Data.Message_Name := +Message_Name;
      Self.Ref.Data.Data := Data;
      Self.Ref.Data.Future := Future;
   end Initialize;

   -----------------------------------------------------------------------------
   function Get_Name(Self : Message_Type) return String is
      use kv.avm.Registers;
   begin
      return +Self.Ref.Data.Message_Name;
   end Get_Name;

   -----------------------------------------------------------------------------
   function Get_Source(Self : Message_Type) return kv.avm.Actor_References.Actor_Reference_Type is
   begin
      return Self.Ref.Data.Source_Actor;
   end Get_Source;

   -----------------------------------------------------------------------------
   function Get_Reply_To(Self : Message_Type) return kv.avm.Actor_References.Actor_Reference_Type is
   begin
      return Self.Ref.Data.Reply_To;
   end Get_Reply_To;

   -----------------------------------------------------------------------------
   function Get_Destination(Self : Message_Type) return kv.avm.Actor_References.Actor_Reference_Type is
   begin
      return Self.Ref.Data.Destination_Actor;
   end Get_Destination;

   -----------------------------------------------------------------------------
   function Get_Data(Self : Message_Type) return kv.avm.Tuples.Tuple_Type is
   begin
      return Self.Ref.Data.Data;
   end Get_Data;

   -----------------------------------------------------------------------------
   function Get_Future(Self : Message_Type) return Interfaces.Unsigned_32 is
   begin
      return Self.Ref.Data.Future;
   end Get_Future;

   -----------------------------------------------------------------------------
   function Image(Self : Message_Type) return String is
   begin
      return "Message<" & Self.Get_Name & ">";
   end Image;

   -----------------------------------------------------------------------------
   function Debug(Ref : Reference_Counted_Message_Access) return String is
      Count : constant String := Natural'IMAGE(Ref.Count);
   begin
      if Ref.Data = null then
         return "null data"&Count;
      else
         return Debug(Ref.Data);
      end if;
   end Debug;

   -----------------------------------------------------------------------------
   function Debug(Self : Message_Type) return String is
   begin
      if Self.Ref = null then
         return "null ref";
      else
         return Debug(Self.Ref);
      end if;
   end Debug;

   -----------------------------------------------------------------------------
   function Reachable(Self : Message_Type) return kv.avm.Actor_References.Sets.Set is
   begin
      return Self.Ref.Data.Data.Reachable;
   end Reachable;

   ----------------------------------------------------------------------------
   function "="(L, R: Message_Type) return Boolean is
   begin
      if L.Ref = Null or R.Ref = Null then
         return False;
      end if;
      if L.Ref.Data = Null or R.Ref.Data = Null then
         return False;
      end if;
      return (L.Ref.Data.all = R.Ref.Data.all);
   end "=";

   ----------------------------------------------------------------------------
   procedure Message_Write(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : in Message_Type) is
   begin
      kv.avm.Actor_References.Actor_Reference_Type'OUTPUT(Stream, Item.Ref.Data.Source_Actor);
      kv.avm.Actor_References.Actor_Reference_Type'OUTPUT(Stream, Item.Ref.Data.Reply_To);
      kv.avm.Actor_References.Actor_Reference_Type'OUTPUT(Stream, Item.Ref.Data.Destination_Actor);
      kv.avm.Registers.String_Type'OUTPUT                (Stream, Item.Ref.Data.Message_Name);
      kv.avm.Tuples.Tuple_Type'OUTPUT                    (Stream, Item.Ref.Data.Data);
      Interfaces.Unsigned_32'OUTPUT                      (Stream, Item.Ref.Data.Future);
   end Message_Write;


   ----------------------------------------------------------------------------
   procedure Message_Read(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : out Message_Type) is
      Source       : kv.avm.Actor_References.Actor_Reference_Type;
      Reply_To     : kv.avm.Actor_References.Actor_Reference_Type;
      Destination  : kv.avm.Actor_References.Actor_Reference_Type;
      Message_Name : kv.avm.Registers.String_Type;
      Data         : kv.avm.Tuples.Tuple_Type;
      Future       : Interfaces.Unsigned_32;
      use kv.avm.Registers;
   begin
      Item.Finalize; -- Clear out the old one, if there was something there.
      -- Even though the parameters to a message's Initialize routine are
      -- in this order, reading the values as parameters to the routine
      -- resulted in an out-of-order read from the Stream.
      Source       := kv.avm.Actor_References.Actor_Reference_Type'INPUT(Stream);
      Reply_To     := kv.avm.Actor_References.Actor_Reference_Type'INPUT(Stream);
      Destination  := kv.avm.Actor_References.Actor_Reference_Type'INPUT(Stream);
      Message_Name := kv.avm.Registers.String_Type'INPUT(Stream);
      Data         := kv.avm.Tuples.Tuple_Type'INPUT(Stream);
      Future       := Interfaces.Unsigned_32'INPUT(Stream);
      Item.Initialize
         (Source       => Source,
          Reply_To     => Reply_To,
          Destination  => Destination,
          Message_Name => +Message_Name,
          Data         => Data,
          Future       => Future);
   end Message_Read;

end kv.avm.Messages;
