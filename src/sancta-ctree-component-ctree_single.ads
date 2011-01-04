with Ada.Calendar,
     Agpl.Xml,
     Sancta.Ctree.Single_Mover,
     Sancta.Component,
     Sancta.Component.Root;
use Sancta,
    Sancta.Component,
    Sancta.Component.Root;

package Sancta.Ctree.Component.Ctree_Single is

   Name                 : aliased constant Component_Name := "ctree_single";

   Log_Section          : constant String := "Sancta.Ctree.Component.ctree_single";

   Option_Here          : constant Option_Attr := "here";
   Option_Near          : constant Option_Attr := "near";
   --  Num of cells that the algorithm considers to be "near" a robot.
   Option_Period        : constant Option_Attr := "period";
   --  Updating period, propagates as player update period

--   Requires_Root_Pose   : aliased constant Internal_Key := "root_pose";
   Requires_Map         : aliased constant Internal_Key := "map";
   Requires_Team        : aliased constant Internal_Key := "team";
   Requires_Plan        : aliased constant Internal_Key := "plan";
   --  of type Task_List
   Requires_Tree        : aliased constant Internal_Key := "tree";
   Requires_Links       : aliased constant Internal_Key := "links";

   Requires_To_Create : constant Internal_Key_Array :=
     (Requires_Map'Access,
      Requires_Plan'Access);

   Requires_To_Run : constant Internal_Key_Array :=
     (Requires_Team'Access,
      Requires_Plan'Access,
      Requires_Tree'Access,
      Requires_Links'Access);

   Provides_Team_Action   : constant Internal_Key := "team_action";
   Provides_Flag          : constant Internal_Key := "done";
   --  True when plan exhausted
   Provides_Pending_Tasks : constant Internal_Key := "pending_tasks";
   Provides_Team_Tree     : constant Internal_Key := "team_tree";
   Provides_Must_Replan   : constant Internal_Key := "must_replan";
   --  Bool saying that head is strained and we need a better plan

   type Object is new Root.Object with private;
   type Object_Access is access all Object;

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node)
                    return      Sancta.Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   type Object is new Root.Object with record
      Mover  : Ctree.Single_Mover.Object_Access;
      Period : Duration := 0.01;
      Next   : Ada.Calendar.Time := Ada.Calendar.Clock;

      Replan_Triggered : Boolean := False;
      Fallbacks        : Natural := 0; -- Times some task has failed
   end record;

end Sancta.Ctree.Component.Ctree_Single;
