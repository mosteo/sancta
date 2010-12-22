with Agpl.Strings.Fields;
with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Component.Helper;
with Sancta.Network;

package body Sancta.Component.Dns is

   package Sn renames Sancta.Network;

   type Object_Access is access all Object;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Node_Info) return Boolean is
   begin
      return L.Id < R.Id;
   end "<";

   -------------------
   -- Add_To_Groups --
   -------------------

   procedure Add_To_Groups (Link   : Network.Layer.Root.Object_Access;
                            Id     : Node_Id;
                            Groups : String)
   is
      use Agpl.Strings.Fields;
   begin
      for I in 1 .. Num_Tokens (Groups) loop
         Link.Add_Node_To_Group (Id, Select_Field (Groups, I));
      end loop;
   end Add_To_Groups;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
      This : constant Object_Access :=
        new Object
          (Name'Access,
           Config,
           SN.Layer.Root.Object_Access
             (Ctypes.Network (Helper.Input (Config, Requires_Link)).Link));

      Never : Ada.Calendar.Time;
   begin
      This.Safe.Init;

      if This.Do_Broadcast then
         This.Safe.Run (Never);
      end if;

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
   begin
      if Boolean'Value (This.Option (Opt_Broadcast, Def_Broadcast'Img)) then
         This.Safe.Run (Next);
      end if;
   end Run;

   ------------------
   -- Do_Broadcast --
   ------------------

   function Do_Broadcast (This : Object) return Boolean is
   begin
      return Boolean'Value (This.Option (Opt_Broadcast, Def_Broadcast'Img));
   end Do_Broadcast;

   ------------------
   -- Purge_Period --
   ------------------

   function Purge_Period (This : Object) return Duration is
   begin
      return Duration'Value
        (This.Option (Opt_Purge_Period, Def_Purge_Period'Img));
   end Purge_Period;

   ------------------
   -- On_Reception --
   ------------------

   procedure On_Reception
     (This : in out Object;
      M    : in     Sancta.Network.Message'Class;
      Meta : in     Sancta.Network.Message_Metadata)
   is
   begin
      if M in Message'Class and then This.Do_Broadcast then
         This.Safe.On_Reception (M, Meta);
      end if;
   end On_Reception;

   ---------------
   -- Safe_Type --
   ---------------

   protected body Safe_Type is

      ----------
      -- Init --
      ----------

      procedure Init is
      begin
         Timer.Set_Period
           (Duration'Value
              (Parent.Option (Opt_Announce_Period, Def_Announce_Period'Img)));

         Channel := Network.Value (Parent.Option (Opt_Channel));
         Parent.Link.Subscribe
           (SN.Consumer.Object_Access (Parent), Channel);
      end Init;

      --------------
      -- Add_Node --
      --------------

      procedure Add_Node (Node : Node_Info) is
      begin
         Nodes.Include (Node);
         Ages.Include (Node.Id, Agpl.Chronos.Clock);
      end Add_Node;

      -------------
      -- Process --
      -------------

      procedure Process (Msg : Message; Link : Network.Layer.Root.Object_Access) is

         ------------------
         -- Check_Update --
         ------------------

         procedure Check_Update is
         begin
            if not Ages.Contains (Msg.Client.Id) then
               Log ("New node: " &
                    Image (Msg.Client.Id) & "@" & (+Msg.Client.Addr) &
                    " in [" & (+Msg.Client.Groups) & "]",
                    Informative, Log_Section);
               Parent.Link.Send_Async
                 (Network.New_Address (Channel),
                  Message'(Kind       => Master_Partial_Update,
                           New_Client => Msg.Client));
               Log ("Partial update sent", Debug, Log_Section);
            end if;

            Parent.Link.Add_Node (Msg.Client.Id, +Msg.Client.Addr);
            Add_To_Groups        (Link, Msg.Client.Id, +Msg.Client.Groups);
            Add_Node             (Msg.Client);
         end Check_Update;

      begin
         Log ("Incoming MSG: " & Msg.Kind'Img, Debug, Log_Section);
         case Msg.Kind is
            when Node_Update =>
               Check_Update;

            when Master_Partial_Update =>
               Log ("New node: " &
                    Image (Msg.New_Client.Id) & "@" & (+Msg.New_Client.Addr) &
                    " in [" & (+Msg.New_Client.Groups) & "]",
                    Informative, Log_Section);
               Link.Add_Node (Msg.New_Client.Id, +Msg.New_Client.Addr);
               Add_To_Groups (Link, Msg.New_Client.Id, +Msg.New_Client.Groups);

            when Master_Full_Update =>
               if not Is_Master then
                  --  Delete dead ones, add new ones...
                  declare
                     procedure Del (I : Network.Node_Sets.Cursor) is
                        Node : constant Node_Id :=
                          Network.Node_Sets.Element (I);
                     begin
                        if not Msg.Nodes.Contains ((Node, others => <>)) then
                           Link.Delete_Node (Node);
                           Log ("Deleting dead node: " & Image (Node),
                                Warning, Log_Section);
                        end if;
                     end Del;

                     procedure Add (I : Node_Sets.Cursor) is
                        Info : constant Node_Info := Node_Sets.Element (I);
                     begin
                        if not Link.Contains (Info.Id) then
                           Log ("New node: " &
                                Image (Info.Id) & "@" & (+Info.Addr) &
                                " in [" & (+Info.Groups) & "]",
                                Informative, Log_Section);
                        else
                           Log ("Old node: " &
                                Image (Info.Id) & "@" & (+Info.Addr) &
                                " in [" & (+Info.Groups) & "]",
                                Debug, Det_Section);
                        end if;
                        Link.Add_Node (Info.Id, +Info.Addr);
                        Add_To_Groups (Link, Info.Id, +Info.Groups);
                     end Add;
                  begin
                     Log ("Full update contains" &
                          Network.Nodes (Link.Everybody).Length'Img & " nodes",
                          Debug, Log_Section);
                     Network.Nodes (Link.Everybody).Iterate (Del'Access);
                     Msg.Nodes.Iterate (Add'Access);
                  end;
               end if;
         end case;
      end Process;

      ---------
      -- Run --
      ---------

      procedure Run (Next :    out Ada.Calendar.Time)
      is

         ----------------
         -- Check_Dead --
         ----------------

         procedure Check_Dead is
            use Node_Ages;
            I : Cursor := Ages.Last;
            J : Cursor;
         begin
            while Has_Element (I) loop
               J := Previous (I);
--                 Log ("AGE: " & Image (Key (I)) & ":" & Element (I).Image, Always);
               if Key (I) /= Parent.Link.Id and then
                 Element (I).Elapsed > Parent.Purge_Period
               then
                  Log ("Deleting dead node: " & Image (Key (I)),
                       Warning, Log_Section);
                  Nodes.Exclude ((Key (I), others => <>));
                  Parent.Link.Delete_Node (Key (I));
                  Ages.Delete   (I);
               end if;
               I := J;
            end loop;
         end Check_Dead;

      begin
         if Has_Self_Address and then not Self_Added then
            Log ("Adding self: " & Sancta.Image (Parent.Link.Id) & "@" &
                 Self_Address,
                 Informative, Log_Section);
            Parent.Link.Add_Node (Parent.Link.Id, Self_Address);
            Add_To_Groups (Parent.Link,
                           Parent.Link.Id,
                           Parent.Option (Opt_Self_Groups, Def_Self_Groups));

            if Is_Master then
               Add_Node
                 ((Id     => Parent.Link.Id,
                   Addr   => +Self_Address,
                   Groups => +Parent.Option (Opt_Self_Groups, Def_Self_Groups)));
            end if;

            Self_Added := True;
         elsif not Self_Added then
            Log ("No self address found yet", Warning, Log_Section);
         end if;

         if Is_Master then
            --  Send full update
            Log ("Master sending full update", debug, log_section);
            Check_Dead;
            Parent.Link.Send_Async
              (Network.New_Address (Channel),
               Message'(Kind  => Master_Full_Update,
                        Nodes => Nodes));
         elsif Has_Self_Address then
            --  Send client info
            Log ("Slave sending update", debug, log_section);
            Parent.Link.Add_Node (Dns_Master_Node,
                                  Parent.Option (Opt_Master_Address));
            Parent.Link.Send
              (Network.New_Address (Dns_Master_Node, Channel),
               Message'(Kind   => Node_Update,
                        Client =>
                          (Id     => Parent.Link.Id,
                           Addr   => +Self_Address,
                           Groups => +Parent.Option
                             (Opt_Self_Groups, Def_Self_Groups))));
            Parent.Link.Delete_Node (Dns_Master_Node);
            --  This pollutes a bit the namespace, but since we need async
            --    sending to prevent blocking exceptions, no way out for now...
            --  It could be workarounded for sure if I get the time, moving
            --  the sending out of the protected
         end if;

         Timer.Next (Next);
      end Run;

      ---------------
      -- Is_Master --
      ---------------

      function Is_Master return Boolean is
      begin
         return
           Parent.Option (Opt_Master_Address, "x") =
           Parent.Option (Opt_Self_Address, "");
      end Is_Master;

      ------------------
      -- Self_Address --
      ------------------

      function Self_Address return String is
      begin
         if Parent.Provided_And_Exists (Requires_Address) then
            return Ctypes.Dstring (Parent.Input (Requires_Address)).Str;
         elsif Parent.Exists (Opt_Self_Address) then
            return Parent.Option (Opt_Self_Address);
         else
            Log ("No address found for DNS", Warning, Log_Section);
            raise Constraint_Error with "No address found at dns";
         end if;
      end Self_Address;

      ----------------------
      -- Has_Self_Address --
      ----------------------

      function Has_Self_Address return Boolean is
      begin
         return Self_Address /= "";
      exception
         when Constraint_Error =>
            return False;
      end Has_Self_Address;

      ------------------
      -- On_Reception --
      ------------------

      procedure On_Reception
        (M    : in     Sancta.Network.Message'Class;
         Meta : in     Sancta.Network.Message_Metadata)
      is
         pragma Unreferenced (Meta);
      begin
         if M in Message'Class then
            Process (Message (M), Parent.Link);
         end if;
      end On_Reception;

   end Safe_Type;

end Sancta.Component.Dns;
