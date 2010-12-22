with Agpl.Streams.Circular_Unbounded;
with Agpl.Streams.Memory_Arrays_Constrained;

package body Sancta.Network is

   package Packet_Streams is new
     Agpl.Streams.Memory_Arrays_Constrained (Max_Message_Size);

   ------------
   -- Create --
   ------------

   function Create (Name : in String) return Group_Id is
   begin
      return
        (Name   => +Name,
         Nodes  => Node_Sets.Empty_Set);
   end Create;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (This : in Group_Id) return String is
   begin
      return To_String (This.Name);
   end Get_Name;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (This : in out Group_Id; Node : in Node_Id) is
   begin
      This.Nodes.Include (Node);
   end Add_Node;

   -----------------
   -- Delete_Node --
   -----------------

   procedure Delete_Node (This : in out Group_Id; Node : Node_Id) is
   begin
      This.Nodes.Exclude (Node);
   end Delete_Node;

   -----------------
   -- Clear_Nodes --
   -----------------

   procedure Clear_Nodes (This : in out Group_Id) is
   begin
      This.Nodes.Clear;
   end Clear_Nodes;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member (This : Group_Id; Node : Node_Id) return Boolean is
   begin
      return This.Nodes.Contains (Node);
   end Is_Member;

   -----------
   -- Nodes --
   -----------

   function Nodes (This : Group_Id) return Node_Sets.Set is
   begin
      return This.Nodes;
   end Nodes;

   ------------
   -- To_Raw --
   ------------
   --  We aren't possibly gaining anything by not doing this as unchecked_conv.
   function To_Raw (This : in Message'Class)
                    return    Ada.Streams.Stream_Element_Array
   is
--        L1  : Logger (new String'("Ntw:Pre-Buf"), Always);
      Buffer : aliased Packet_Streams.Sized_Array;
--        L2  : Logger (new String'("Ntw:Pre-Str"), Always);
      Stream : aliased Packet_Streams.Stream_Type (Buffer'Access);
--        L3  : Logger (new String'("Ntw:Pst-Str"), Always);
   begin
      Message'Class'Output (Stream'Access, This);
      return Buffer (Buffer'First .. Stream.Index);
   end To_Raw;

   ----------------
   -- To_Message --
   ----------------

   function To_Message (This : in Ada.Streams.Stream_Element_Array)
                        return    Message'Class
   is
      Stream : aliased Agpl.Streams.Circular_Unbounded.Stream_Type;
   begin
      Stream.Create;
      Stream.Write (This);
      return Message'Class'Input (Stream'Access);
   exception
      when E : others =>
         Log ("Network.To_Message: " & Report (E) &
              " for message of size" & This'Length'Img, Error, Log_Section);
         raise;
   end To_Message;

   ------------
   -- Sender --
   ------------

   function Sender (This : in Message_Metadata) return Node_Id
   is
   begin
      return This.Sender;
   end Sender;

   --------------
   -- Receiver --
   --------------

   function Receiver (This : in Message_Metadata) return Address'Class is
   begin
      return This.Receiver;
   end Receiver;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Group_Id) return Boolean is
      use Asu;
   begin
      return L.Name = R.Name;
   end "=";

   -----------
   -- Image --
   -----------

   function Image (Chan : Channel) return String is
   begin
      return + Ustring (Chan);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Str  : String)  return Channel is
   begin
      return Channel (Ustring'(+Str));
   end Value;

   -----------------
   -- New_Address --
   -----------------

   function New_Address (Dest : Node_Id;
                         Chan : Channel) return Address is
   begin
      return (Kind      => Unicast,
              Chan      => Chan,
              Dest_Node => Dest);
   end New_Address;

   -----------------
   -- New_Address --
   -----------------

   function New_Address (Dest : Group_Id;
                         Chan : Channel) return Address is
   begin
      return (Kind       => Multicast,
              Chan       => Chan,
              Dest_Group => Dest);
   end New_Address;

   -----------------
   -- New_Address --
   -----------------

   function New_Address (Chan : Channel) return Address is
   begin
      return (Kind => Broadcast,
              Chan => Chan);
   end New_Address;

   ----------
   -- Kind --
   ----------

   function Kind (This : Address) return Address_Kinds is
   begin
      return This.Kind;
   end Kind;

   ----------
   -- Node --
   ----------

   function Node (This : Address) return Node_Id is
   begin
      return This.Dest_Node;
   end Node;

   -----------
   -- Group --
   -----------

   function Group (This : Address) return Group_Id is
   begin
      return This.Dest_Group;
   end Group;

   ------------------
   -- Intended_For --
   ------------------

   function Intended_For (This : Address; Id : Node_Id) return Boolean is
   begin
      return
        This.Kind = Broadcast or else
        (This.Kind = Unicast and then This.Dest_Node = Id) or else
        (This.Kind = Multicast and then Is_Member (This.Dest_Group, Id));
   end Intended_For;

   -----------
   -- Image --
   -----------

   function Image (This : Address) return String is
   begin
      case This.Kind is
         when Broadcast =>
            return "Address broadcast";
         when Multicast =>
            return "Address multicast: " & Image (This.Group);
         when Unicast =>
            return "Address unicast: " & Image (This.Node);
      end case;
   end Image;

end Sancta.Network;
