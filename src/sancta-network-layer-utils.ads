with Agpl.Xml,
     Sancta.Network.Layer.Root;

package Sancta.Network.Layer.Utils is

   --  Support functions

   Log_Section : constant String := "sancta.network.layer.utils";

   procedure Configure (This : in out Root.Object'Class;
                        Conf :        Agpl.Xml.Node);
   pragma Precondition (This.Id /= No_Node);
   --  Conf must point to a Network Component.
   --  This procedure adds nodes, creates groups.
   --  Id must be already set.

   procedure Add_Nodes (This : in out Root.Object'Class;
                        Conf :        Agpl.Xml.Node);
   pragma Precondition (This.Id /= No_Node);

   procedure Add_Groups (This : in out Root.Object'Class;
                         Conf :        Agpl.Xml.Node);
   pragma Precondition (This.Id /= No_Node);

end Sancta.Network.Layer.Utils;
