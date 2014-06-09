--  A complete black box that executes hungarian+propagate+hungarian strategy
--  for limited connectivity.

with Ada.Calendar,
     Sancta.Ctree.Assigner.Tspopt,
     Sancta.Ctree.Connectivity_Matrix,
     Sancta.Assignment,
     Sancta.Component,
     Sancta.Component.Root,
     Sancta.Containers,
     Sancta.Cost_Cache,
     Sancta.Types;

use Sancta.Component,
    Sancta.Containers,
    Sancta;

package Sancta.Ctree.Component.Scluster_Tspopt is

   Log_Section : constant String := "Sancta.Ctree.Component.scluster_tspopt";

   Name : aliased constant Component_Name := "scluster_tspopt";

   Requires_Pending_Tasks : constant Internal_Key := "pending_tasks";
   --  TC.Lists.List

   Requires_Costs         : constant Internal_Key := "costs";
   --  Cost_Cache'Class

   Requires_Links         : constant Internal_Key := "links";
   --  NCTypes.Clusters (Connectivity_Matrix)

   Requires_Team          : constant Internal_Key := "in_team";
   --  CTypes.Team

   Provides_Team          : constant Internal_Key := "out_team";
   --  Modified with the appropriate assignation

   Provides_Scluster      : constant Internal_Key := "scluster";
   --  Connectivity matrix with the S-Cluster (aggregated clusters)

   Option_Base_Name       : constant Option_Attr := "base_name";
   --  If given, this robot will be unused for allocation.

   procedure Register;

   --  COMPONENT BLACK BOX ENDS HERE  --

   type Proto is tagged private;
   --  This type provides the exact same functionality as the component, but
   --   stand-alone

   procedure Run (This  : in out Proto;
                  Bots  : in out Assignment.Object;
                  Jobs  : in out Tc.Lists.List;
                  Costs :        Cost_Cache.Object'Class;
                  Links :        Connectivity_Matrix.Object'Class);

private

   type Proto is tagged record
      Tsp       : Assigner.Tspopt.Object;
      Sclusters : Connectivity_Matrix.Object;
      Base_Name : Node_Id;

      Prev_Jobs : Tc.Lists.List;
      Prev_Ass  : Assignment.Object;

      Goal_Distance : Types.Real := 1.0;
   end record;

   type Object is new Root.Object with record
      Inner : Proto;
      Ready : Boolean := False;
   end record;

   function Create (Config : in Comp_Config)
                    return      Sancta.Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Ctree.Component.Scluster_Tspopt;
