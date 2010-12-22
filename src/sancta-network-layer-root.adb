with Agpl.Conversions;
with Agpl.Tasking.Code;
with Agpl.Tasking.Workers;
with Sancta.Network.Handles;

package body Sancta.Network.Layer.Root is

   ---------------
   -- Safe_Type --
   ---------------

   protected body Safe_Type is

      ------------
      -- Get_Id --
      ------------

      function Get_Id return Node_Id is
      begin
         return Id;
      end Get_Id;

      ------------
      -- Set_Id --
      ------------

      procedure Set_Id (Id : Node_Id) is
      begin
         Safe_Type.Id := Id;
      end Set_Id;

      ----------------
      -- Get_Config --
      ----------------

      function Get_Config return Options_Access is
      begin
         return Config'Unchecked_Access;
      end Get_Config;

      ----------------
      -- Set_Config --
      ----------------

      procedure Set_Config (Config : Options) is
      begin
         Safe_Type.Config := Config;
      end Set_Config;

      -------------------
      -- Get_Everybody --
      -------------------

      function Get_Everybody return Group_Id is
      begin
         return Everybody;
      end Get_Everybody;

      -------------------
      -- Get_Consumers --
      -------------------

      function Get_Consumers return Consumer_Maps.Map is
      begin
         return Consumers;
      end Get_Consumers;

      ---------------------
      -- Clear_Consumers --
      ---------------------

      procedure Clear_Consumers is
      begin
         Consumers.Clear;
      end Clear_Consumers;

      -----------------------
      -- Add_Node_To_Group --
      -----------------------

      procedure Add_Node_To_Group
        (Node     :        Node_Id;
         Group    :        Group_Name)
      is
         G : Group_Id;
      begin
         Add_Node (Everybody, Node);

         if Groups.Contains (Group) then
            G := Groups.Element (Group);
         else
            G := Create (Group);
         end if;
         Add_Node (G, Node);
         Groups.Include (Group, G);
      end Add_Node_To_Group;

      ----------------------------
      -- Delete_Node_From_Group --
      ----------------------------

      procedure Delete_Node_From_Group (Node : Node_Id; Group : Group_Name) is
         G : Group_Id;
      begin
         if Groups.Contains (Group) then
            G := Groups.Element (Group);
            Delete_Node (G, Node);
            Groups.Include (Group, G);
         end if;
      end Delete_Node_From_Group;

      ---------------
      -- Get_Group --
      ---------------

      function Get_Group (Group : Group_Name) return Group_Id is
      begin
         if Groups.Contains (Group) then
            return Groups.Element (Group);
         else
            return Create (Group);
         end if;
      end Get_Group;

      ---------------
      -- Subscribe --
      ---------------

      procedure Subscribe
        (Listener :        Consumer.Object_Access;
         Chan     :        Channel)
      is
         use Consumer_Maps;
         use Consumer_Lists;
         I : constant Consumer_Maps.Cursor := Find (Consumers, Chan);
         L :          Consumer_Lists.List;
         X : constant Consumer.Object_Access := Listener;
      begin
         if Has_Element (I) then
            L := Element (I);
            if not Contains (L, X) then
               Append (L, X);
            end if;
         else
            Append (L, X);
         end if;

         Consumers.Include (Chan, L); -- Replace old list.
      end Subscribe;

      -----------------
      -- Clear_Nodes --
      -----------------

      procedure Clear_Nodes is
      begin
         Clear_Nodes (Everybody);
      end Clear_Nodes;

      --------------
      -- Add_Node --
      --------------

--        procedure Add_Node (Node : Node_Id) is
--        begin
--           Add_Node (Everybody, Node);
--        end Add_Node;

      -----------------
      -- Delete_Node --
      -----------------

      procedure Delete_Node (Node : Node_Id) is
      begin
         Delete_Node (Everybody, Node);
      end Delete_Node;

      --------------
      -- Contains --
      --------------

      function Contains (Node : Node_Id) return Boolean is
      begin
         return Id_Addr.Contains (Node) and then Is_Member (Everybody, Node);
      end Contains;

      --------------
      -- Add_Node --
      --------------

      procedure Add_Node
        (Node :        Node_Id;
         Addr :        Protocol_Specific_Address)
      is
      begin
         Add_Node        (Everybody, Node);
         Id_Addr.Include (Node, Addr);
         Addr_Id.Include (Addr, Node);
      end Add_Node;

      -----------------
      -- Get_Node_Id --
      -----------------

      function Get_Node_Id (Addr : Protocol_Specific_Address)
                            return Node_Id is
      begin
         return Addr_Id.Element (Addr);
      end Get_Node_Id;

      -----------------
      -- Get_Address --
      -----------------

      function Get_Address (Id   : Node_Id) return Protocol_Specific_Address is
      begin
         return Id_Addr.Element (Id);
      end Get_Address;

      ---------------
      -- Num_Nodes --
      ---------------

      function Num_Nodes return Natural is
      begin
         return Natural (Id_Addr.Length);
      end Num_Nodes;

      ----------------
      -- Node_Names --
      ----------------

      function Node_Names return Node_Set is
         Names : Node_Set;

         procedure Add (I : Id_Addr_Maps.Cursor) is
            Id : constant Node_Id := Id_Addr_Maps.Key (I);
         begin
            Names.Include (Id);
         end Add;

      begin
         Id_Addr.Iterate (Add'Access);
         return Names;
      end Node_Names;

   end Safe_Type;

   ----------
   -- Send --
   ----------

   procedure Send
     (This : in out Object;
      Dest : in     Address;
      Data : in     Message'Class)
   is
      Pkt : constant Packets.Packet :=
        Packets.Create_Packet (Data, This.Id, Dest);
      Raw : constant Stream_Element_Array := Packets.To_Raw (Pkt);

      procedure Send (Dest : Node_Id; Raw : Stream_Element_Array) is
      begin
         if This.Safe.Contains (Dest) then
            Log ("Sending to " & Image (Dest) & ": " &
                 External_Tag (Data'Tag),
                 Debug, Log_Section);
            if This.Config.Count_Own_Bw or else
              Dest /= This.Id
            then
               This.Bw_Out.Push (Bw_Unit (Raw'Length));
            end if;
            Object'Class (This).Send (Dest, Raw);
         else
            raise Constraint_Error with "Unknown address: " & Image (Dest);
         end if;
      end Send;

      procedure Send (I : Node_Sets.Cursor) is
         Dest : Node_Id renames Node_Sets.Element (I);
      begin
         Send (Dest, Raw);
      end Send;
   begin
      case Dest.Kind is
         when Unicast =>
            Send (Dest.Node, Raw);
         when Multicast =>
            Dest.Group.Nodes.Iterate (Send'Access);
         when Broadcast =>
            This.Safe.Get_Everybody.Nodes.Iterate (Send'Access);
      end case;
   end Send;

   -----------------
   -- Clear_Nodes --
   -----------------

   procedure Clear_Nodes (This : in out Object) is
   begin
      This.Safe.Clear_Nodes;
   end Clear_Nodes;

   --------------
   -- Add_Node --
   --------------

--     procedure Add_Node
--       (This : in out Object;
--        Node :        Node_Id) is
--     begin
--        This.Safe.Add_Node (Node);
--     end Add_Node;

   -----------------------
   -- Add_Node_To_Group --
   -----------------------

   procedure Add_Node_To_Group
     (This     : in out Object;
      Node     :        Node_Id;
      Group    :        Group_Name)
   is
   begin
      This.Safe.Add_Node_To_Group (Node, Group);
   end Add_Node_To_Group;

   -----------------
   -- Delete_Node --
   -----------------

   procedure Delete_Node (This : in out Object; Node : Node_Id) is
   begin
      This.Safe.Delete_Node (Node);
   end Delete_Node;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group (This  : Object;
                       Group : Group_Name) return Group_Id
   is
   begin
      return This.Safe.Get_Group (Group);
   end Get_Group;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe
     (This     : in out Object;
      Listener :        Consumer.Object_Access;
      Chan     :        Channel)
   is
   begin
      This.Safe.Subscribe (Listener, Chan);
   end Subscribe;

   ------------
   -- Config --
   ------------

   function Config (This : Object'Class) return Options is
   begin
      return This.Safe.Get_Config.all;
   end Config;

   ----------------
   -- Set_Config --
   ----------------

   procedure Set_Config (This : in out Object'Class; Config : Options) is
   begin
      This.Safe.Set_Config (Config);
   end Set_Config;

   -------------------------------
   -- Set_Asynchronous_Dispatch --
   -------------------------------

   procedure Set_Asynchronous_Dispatch
     (This    : in out Options;
      Enabled :        Boolean := False) is
   begin
      This.Async_Dispatch := Enabled;
   end Set_Asynchronous_Dispatch;

   ----------------------
   -- Set_Count_Own_BW --
   ----------------------

   procedure Set_Count_Own_BW
     (This    : in out Options;
      Enabled :        Boolean := False) is
   begin
      This.Count_Own_BW := Enabled;
   end Set_Count_Own_BW;

   type Dispatcher is new Agpl.Tasking.Code.Object with record
      C  : Consumer.Object_Access;
      M  : Message_Handle;
      Md : Handles.Metadata_Handle;
   end record;

   overriding
   procedure Run (D : in out Dispatcher);
   procedure Run (D : in out Dispatcher) is
   begin
      D.C.On_Reception (D.M.Ref.all, D.Md.Ref.all);
      Log ("Registered message dispatched [async] 4chan: " &
           Image (D.Md.Ref.Receiver.Chan) & "; Tag: " &
           External_Tag (D.M.Ref.all'Tag),
           Debug, Section => Log_Section);
   end Run;

   -----------------------------
   -- Dispatch_To_Subscribers --
   -----------------------------

   procedure Dispatch_To_Subscribers
     (This : Object;
      Cons : Consumer_Maps.Map;
      P    : Packet)
   is
      M  : constant Message'Class    := To_Message (Data (P));
      Md : constant Message_Metadata := Metadata (P);

      procedure Call (X : in Consumer_Lists.Cursor) is
         use Handles;
      begin
         if This.Safe.Get_Config.Async_Dispatch then
            Agpl.Tasking.Workers.Launch
              (Dispatcher'
                 (Consumer_Lists.Element (X),
                  Set (M), Set (Md)));
         else
            Consumer.On_Reception (Consumer_Lists.Element (X).all, M, Md);
            Log ("Registered message dispatched [sync] 4chan: " &
                 Image (Md.Receiver.Chan) & "; Tag: " &
                 External_Tag (M'Tag),
                 Debug, Section => Log_Section);
         end if;
      exception
         when E : others =>
            Log ("Dispatching received message 4chan: " &
                 Image (Md.Receiver.Chan) & "; Tag: " &
                 External_Tag (M'Tag) & "; Except: " &
                 Report (E),
                 Error, Log_Section);
      end Call;

      use Consumer_Maps;
      I : constant Consumer_Maps.Cursor :=
            Cons.Find (Md.Receiver.Chan);
   begin
      if Has_Element (I) then
         declare
            L : constant Consumer_Lists.List := Element (I);
         begin
            if not L.Is_Empty then
               L.Iterate (Call'Access);
            else
               Log ("Unregistered message discarded for channel: " &
                    Image (Md.Receiver.Chan) & "; Tag: " &
                    External_Tag (M'Tag),
                    Debug, Section => Log_Section);
            end if;
         end;
      end if;
   end Dispatch_To_Subscribers;

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id (This : in out Object;
                     Id   :        Node_Id)
   is
   begin
      This.Safe.Set_Id (Id);
   end Set_Id;

   --------
   -- Id --
   --------

   function Id (This : Object) return Node_Id is
   begin
      return This.Safe.Get_Id;
   end Id;

   --------------
   -- Receiver --
   --------------

   task body Receiver is
      Done : Boolean := False;

      This : Object'Class renames Object'Class (Parent.all);
   begin
      select
         accept Start;
      or
         terminate;
      end select;

      loop
         begin
            declare
               Raw : constant Stream_Element_Array := This.Receive;
               P   : constant Packet               := To_Packet (Raw);
            begin
               if This.Config.Count_Own_Bw or else
                  Metadata (P).Sender /= This.Id
               then
                  This.Bw_In.Push (Bw_Unit (Raw'Length));
               end if;
               Log ("Packet received with length" & Raw'Length'Img,
                    Debug, Section => Log_Section);
               Log ("Packet is from " & Image (Network.Sender (Metadata (P))),
                    Debug, Section => Log_Section);
               This.Dispatch_To_Subscribers
                 (This.Safe.Get_Consumers, To_Packet (Raw));
            end;
         exception
            when Shutting_Down =>
               Done := True;
               Log ("network.layer.root.receiver: Shutting down.",
                    Debug);
            when Interrupted_Reception =>
               Log ("network.layer.root.receiver: Interrupted I/O",
                    Debug);
            when E : others =>
               Log ("network.layer.root.receiver: " & Report (E),
                    Warning, Log_Section);
         end;
         exit when Done;
      end loop;
   end Receiver;

   ---------------------
   -- Start_Listening --
   ---------------------

   procedure Start_Listening (This : in out Object) is
   begin
      This.Recv.Start;
   end Start_Listening;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (This : in out Object) is
   begin
      This.Safe.Clear_Consumers;
      This.The_End := True;
   end Shutdown;

   -----------------
   -- Bw_Informer --
   -----------------

   task body Bw_Informer is
      This : Object renames Parent.all;
      use Agpl.Conversions;
   begin
      while not This.The_End loop
         declare
            Avg_In  : Bw_Unit;
            Avg_Out : Bw_Unit;
         begin
            This.Bw_In.Average  (Avg_In);
            This.Bw_Out.Average (Avg_Out);

            Log ("BW In : " & To_String (Float (Avg_In), Decimals => 1) &
                 " (" & To_Size (Float (Avg_In), Decimals => 1) & ")/s",
                 Informative, Bw_Section);
            Log ("BW Out: " & To_String (Float (Avg_Out), Decimals => 1) &
                 " (" & To_Size (Float (Avg_Out), Decimals => 1) & ")/s",
                 Informative, Bw_Section);
         end;
         delay 1.0;
      end loop;
   end Bw_Informer;

   ---------------
   -- Avg_Bw_In --
   ---------------

   function Avg_Bw_In (This : Object'Class) return Float is
      Avg : Bw_Unit;
   begin
      This.Self.Bw_In.Average (Avg);
      return Float (Avg);
   end Avg_Bw_In;

   ----------------
   -- Avg_Bw_Out --
   ----------------

   function Avg_Bw_Out (This : Object'Class) return Float is
      Avg : Bw_Unit;
   begin
      This.Self.Bw_Out.Average (Avg);
      return Float (Avg);
   end Avg_Bw_Out;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node
     (This : in out Object;
      Node :        Node_Id;
      Addr :        Protocol_Specific_Address)
   is
   begin
      This.Safe.Add_Node (Node, Addr);
   end Add_Node;

   -----------------
   -- Get_Node_Id --
   -----------------

   function Get_Node_Id (This : Object;
                         Addr : Protocol_Specific_Address)
                         return Node_Id is
   begin
      return This.Safe.Get_Node_Id (Addr);
   end Get_Node_Id;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address (This : Object;
                         Id   : Node_Id) return Protocol_Specific_Address is
   begin
      return This.Safe.Get_Address (Id);
   end Get_Address;

   --------------
   -- Contains --
   --------------

   function Contains (This : Object; Id : Node_Id) return Boolean is
   begin
      return This.Safe.Contains (Id);
   end Contains;

   ---------------
   -- Num_Nodes --
   ---------------

   function Num_Nodes (This : Object) return Natural is
   begin
      return This.Safe.Num_Nodes;
   end Num_Nodes;

   ----------------
   -- Node_Names --
   ----------------

   function Node_Names (This : Object) return Node_Set is
   begin
      return This.Safe.Node_Names;
   end Node_Names;

   ---------------
   -- Everybody --
   ---------------

   function Everybody (This : Object) return Group_Id is
   begin
      return This.Safe.Get_Everybody;
   end Everybody;

end Sancta.Network.Layer.Root;
