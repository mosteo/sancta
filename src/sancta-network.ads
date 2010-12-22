with Agpl.Generic_Handle;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

--   with Ada.Containers.Doubly_Linked_Lists,
with Ada.Containers.Ordered_Sets;
with Ada.Streams;

package Sancta.Network is

   pragma Preelaborate;

   Log_Section : constant String := "sancta.network";

   type Message is abstract tagged null record;
   --  Not interface because bugs in GPL2008/GPL2009
   --  Messages will be sent via 'Output writting.
   --  Thus, any byte ordering or language-independency must be managed
   --  overriding the 'Read and 'Write attributes.
   --  Define new messages via regular OO extension.

   Max_Message_Size : constant := 16 * 1024;
   --  Max data to send in a single message.
   --  Buffers of this size are allocated in the stack.
   --  So beware!! of stacks overflow.
   --  Done this way because stack is quicker than heap...

   package Message_Handles is new Agpl.Generic_Handle (Message'Class);
   type Message_Handle is new Message_Handles.Object with null record;

   function To_Raw (This : in Message'Class)
                    return    Ada.Streams.Stream_Element_Array;
   --  Used to easily have what to send to any stream.

   function To_Message (This : in Ada.Streams.Stream_Element_Array)
                        return    Message'Class;
   --  Reconstruction of a message.

   --  A node is given a unique arbitrary id. This must be mapped somehow
   --  by the network layer to real addresses apt for the medium in use.

   function Image (Id : in Node_Id) return String  renames Sancta.Image;
   function Value (Id : in String)  return Node_Id renames Sancta.Value;
   --  Avoid the need for "with"ing Sancta

   package Node_Sets is new Ada.Containers.Ordered_Sets (Node_Id);
   subtype Node_Set is Node_Sets.Set;

   type Group_Id is private;
   --  Group address is for multicast.

   subtype Group_Name is String;

   function "=" (L, R : Group_Id) return Boolean;

   function Create (Name : in Group_Name) return Group_Id;
   function Value  (Name : in Group_Name) return Group_Id renames Create;

   function Get_Name (This : in Group_Id) return String;
   function Image    (This : in Group_Id) return String renames Get_Name;
   --  Name of the group used in Create.

   procedure Add_Node (This : in out Group_Id; Node : in Node_Id);
   --  Add a member to a group.

   procedure Delete_Node (This : in out Group_Id; Node : Node_Id);

   procedure Clear_Nodes (This : in out Group_Id);

   function Is_Member (This : Group_Id; Node : Node_Id) return Boolean;

   function Nodes (This : Group_Id) return Node_Sets.Set;

   type Channel is private;
   --  Destination of messages (equivalent to ports...)

   function Image (Chan : Channel) return String;
   function Value (Str  : String)  return Channel;

   type Address_Kinds is (Unicast, Multicast, Broadcast);
   for Address_Kinds'Size use Ada.Streams.Stream_Element'Size;
   --  More packing in order to reduce BW overhead

   type Address (<>) is tagged private;

   function New_Address (Dest : Node_Id;
                         Chan : Channel) return Address;

   function New_Address (Dest : Group_Id;
                         Chan : Channel) return Address;

   function New_Address (Chan : Channel) return Address;
   --  Broadcast

   function Kind (This : Address) return Address_Kinds;

   function Node (This : Address) return Node_Id;
   --  For unicast

   function Group (This : Address) return Group_Id;
   --  For multicast

   function Intended_For (This : Address; Id : Node_Id) return Boolean;
   --  Id is recipient or belongs to its group

   type Message_Metadata (<>) is tagged private;
   --  Metadata contains sender, receiver, etc.

   function Sender   (This : in Message_Metadata) return Node_Id;
   function Receiver (This : in Message_Metadata) return Address'Class;

   function Image (This : Address) return String;

   subtype Protocol_Specific_Address is String;

private

   type Channel is new Ustring;

   type Address (Kind : Address_Kinds) is
     tagged record
         Chan : Channel;
         case Kind is
            when Unicast =>
               Dest_Node : Node_Id;
            when Multicast =>
               Dest_Group : Group_Id;
            when Broadcast =>
               null;
         end case;
   end record;
   --  for Address'External_Tag use "a";
   --  Reduce overhead of sending addresses by stream

   --   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists (Node_Id);

   type Group_Id is record
      Nodes : Node_Sets.Set;
      Name  : Ustring;
   end record;

   type Message_Metadata (Kind : Address_Kinds) is tagged record
      Sender   : Node_Id;
      Receiver : Address (Kind);
   end record;
   --  for Message_Metadata'External_Tag use "mm";
   --  Only two bytes of overhead for sending metadata by stream
   --  This rep. claus. is not really needed because Network.Packets uses
   --    non class-wide objects, thus removing the tag from the stream.

end Sancta.Network;
