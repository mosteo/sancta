--  Sample empty component to save-as when creating new ones.

with Sancta.Ctree.Stats,
     Sancta.Component,
     Sancta.Component.Root,
     Sancta.Containers;

use Sancta.Component,
    Sancta.Component.Root;

package Sancta.Ctree.Component.Stats is

   Log_Section : constant String := "Sancta.Ctree.Component.stats";

   Name : aliased constant Component_Name := "nerus_stats";

   Requires_Team          : constant Internal_Key := "team"; -- Ass
   Requires_Links         : constant Internal_Key := "links"; -- Conn.Matrix
   Requires_Pending_Tasks : constant Internal_Key := "pending_tasks";

   procedure Register;

private

   use Sancta.Containers;

   type Object is new Root.Object with record
      Tracker : Sancta.Ctree.Stats.Object;
      Inited  : Boolean := False;
      Tasks   : Tc.Lists.List;
   end record;

   function Create (Config : Comp_Config)
                    return   Sancta.Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   overriding
   procedure Stop (This : in out Object);

end Sancta.Ctree.Component.Stats;
