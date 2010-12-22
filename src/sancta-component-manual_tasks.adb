with Agpl.Xml,
     Agpl.Xml.Utils,
     Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Containers,
     Sancta.Tasks.Goto_Pose,
     Sancta.Types;

use Sancta.Containers;

pragma Elaborate_All (Agpl.Xml);

package body Sancta.Component.Manual_Tasks is

   function Get_Attribute is new
     Agpl.Xml.Get_Generic_Attribute
       (Sancta.Types.Real, Sancta.Types.Real'Image, Sancta.Types.Real'Value);

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
      use Agpl.Xml, Agpl.Xml.Utils;
      This : constant Object_Access := new Object (Name'Access, Config);

      Result : Tc.Lists.List;
      Nodes  : constant Node_Vector := Get_All (Config, "task");
   begin
      for I in Nodes.First_Index .. Nodes.Last_Index loop
         if Get_Attribute (Nodes.Element (I), "enabled", True) then
            Result.Append
              (Tasks.Goto_Pose.Create
                 ((Get_Attribute (Nodes.Element (I), "x", 0.0),
                   Get_Attribute (Nodes.Element (I), "y", 0.0),
                   0.0)));
         end if;
      end loop;

      This.Output (Provides_Tasks, Types.Task_List'(Data with Result));
      return Component.Object_Access (This);
   end Create;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Manual_Tasks;
