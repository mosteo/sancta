with Ada.Calendar,
     Agpl.Xml,
     Sancta.Ctree.Connectivity_Matrix,
     Sancta.Component,
     Sancta.Component.Root,
     Sancta.Containers,
     Sancta.Map;
use Sancta,
    Sancta.Component,
    Sancta.Component.Root,
    Sancta.Containers;

package Sancta.Ctree.Component.Ctree_Draw is

   --  Draw everything

   Name                 : aliased constant Component_Name := "ctree_draw";

   Log_Section          : constant String := "Sancta.Ctree.Component.ctree_draw";

   Option_Period        : constant Option_Attr := "period";

   Requires_Map         : aliased constant Internal_Key := "map";
   Requires_Team_Tree   : aliased constant Internal_Key := "team_tree";
   --  Types.Team_Tree
   Requires_Tasks       : aliased constant Internal_Key := "tasks";
   Requires_Tree        : aliased constant Internal_Key := "tree";
   --  CTree.Tree_Navigator
   Requires_Links       : aliased constant Internal_Key := "links";

   Requires_All         : constant Internal_Key_Array :=
                            (Requires_Map'Access,
                             Requires_Team_Tree'Access,
                             Requires_Tasks'Access,
                             Requires_Tree'Access,
                             Requires_Links'Access);

   Provides_Queue_Mesh  : constant Internal_Key := "queue_mesh";
   Provides_Queue_Tasks : constant Internal_Key := "queue_tasks";
   --  Separate names in order to selectively disable

   type Object is new Root.Object with private;
   type Object_Access is access all Object;

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node)
                    return      Sancta.Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   not overriding
   procedure Do_Draw (This : in out Object);

   type Object is new Root.Object with record
      Period      : Duration := 0.1;
      First_Time  : Boolean  := True;

      Curr_Branch,
      Prev_Branch : Map.Path;

      Curr_Tasks,
      Prev_Tasks  : Tc.Lists.List;

      Links       : Connectivity_Matrix.Object;

      Busy        : Boolean := False;
   end record;

end Sancta.Ctree.Component.Ctree_Draw;
