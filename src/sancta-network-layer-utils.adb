with Agpl.Strings;
with Sancta.Component.Helper;

package body Sancta.Network.Layer.Utils is

   use Agpl.Strings;
   use Agpl.Xml;

   ---------------
   -- Add_Nodes --
   ---------------

   procedure Add_Nodes
     (This : in out Root.Object'Class;
      Conf :        Agpl.Xml.Node)
   is
      Nodes_Enabled : constant String :=
        Trim (L (Component.Helper.Option (Conf, "nodes", "")));

      Nodes : constant Node_Vector := Get_All (Conf, "node");
   begin
      Log ("Adding nodes...", Debug, Log_Section);

      if Nodes_Enabled = "" then
         Log ("No 'nodes' attr supplied, adding ALL nodes.",
              Warning, Log_Section);
      else
         Log ("Enabled nodes: " & Nodes_Enabled,
              Informative, Log_Section);
      end if;

      for I in Nodes.First_Index .. Nodes.Last_Index loop
         declare
            Id   : constant String :=
              Get_Attribute (Nodes.Element (I), "id");
            Addr : constant String :=
              Get_Attribute (Nodes.Element (I), "address");
         begin
            if Nodes_Enabled = "" or else Contains (Nodes_Enabled, L (Id)) then
               if Id = "" or else Addr = "" then
                  raise Constraint_Error
                    with "Incomplete node/addr info in network component";
               end if;
               Log ("Node Id/Addr ENABLED: " & Id & "/" & Addr,
                    Debug, Log_Section);
               This.Add_Node (Value (Id), Addr);
            elsif Nodes_Enabled /= "" then
               Log ("Node Id/Addr DISABLED: " & Id & "/" & Addr,
                    Debug, Log_Section);
            end if;
         end;
      end loop;
   end Add_Nodes;

   ----------------
   -- Add_Groups --
   ----------------

   procedure Add_Groups
     (This : in out Root.Object'Class;
      Conf :        Agpl.Xml.Node)
   is
      Nodes_Enabled : constant String :=
        Trim (L (Get_Attribute (Conf, "nodes", "")));

      Groups : constant Node_Vector := Get_All (Conf, "group");
   begin
      Log ("Building groups...", Debug, Log_Section);

      for I in Groups.First_Index .. Groups.Last_Index loop
         declare
            Group   : constant Group_Name :=
              Get_Attribute (Groups.Element (I), "id");
            Members : constant Node_Vector :=
              Get_All (Groups.Element (I), "member");
         begin
            Log ("New group: " & Group, Debug, Log_Section);
            if Group = "" then
               raise Constraint_Error with "Missing group name";
            end if;
            for J in Members.First_Index .. Members.Last_Index loop
               if Nodes_Enabled = "" or else
                 Contains (Nodes_Enabled,
                           L (Get_Attribute (Members.Element (J), "id")))
               then
                  Log ("Grouping: " &
                       Get_Attribute (Members.Element (J), "id") &
                       " ENABLED belongs to " & Group,
                       Debug, Log_Section);
                  This.Add_Node_To_Group
                    (Group => Group,
                     Node  => Value (Get_Attribute (Members.Element (J), "id")));
               elsif Nodes_Enabled /= "" then
                  Log ("Grouping: " &
                       Get_Attribute (Members.Element (J), "id") &
                       " DISABLED belongs to " & Group,
                       Debug, Log_Section);
               end if;
            end loop;
            Log ("End group: " & Group, Debug, Log_Section);
         end;
      end loop;
   end Add_Groups;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This : in out Root.Object'Class;
      Conf :        Agpl.Xml.Node)
   is
   begin
      Add_Nodes  (This, Conf);
      Add_Groups (This, Conf);
   end Configure;

end Sancta.Network.Layer.Utils;
