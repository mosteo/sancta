with Agpl.Xml,
     Sancta.Agent_Proxy,
     Sancta.Component,
     Sancta.Component.Root;

use Sancta.Component,
    Sancta.Component.Root;

package Sancta.Ctree.Component.Ad_Hoc_Cost_Generator is

   --  Attaches a cost generator to a required agent_proxy than considers
   --  both ways in a Explore_Directed_Segment

   Log_Section : constant String := "Sancta.Ctree.Component.ad_hoc_cost_generator";

   Name : aliased constant Component_Name := "nerus_cost_generator";

   Requires_Agent : constant Internal_Key := "agent";

   procedure Register;

private

   use Sancta;

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Agent  :                 Agent_Proxy.Object_Access)
     is new Root.Object (Name, Config) with null record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

end Sancta.Ctree.Component.Ad_Hoc_Cost_Generator;
