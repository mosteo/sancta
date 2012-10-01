with Ada.Calendar,
     Agpl.Xml,
     Sancta.Component,
     Sancta.Component.Root;
use Sancta,
    Sancta.Component,
    Sancta.Component.Root;

package Sancta.Ctree.Component.Scluster_Draw is

   --  Draw everything

   Name            : aliased constant Component_Name := "scluster_draw";

   Log_Section     : constant String := "Sancta.Ctree.Component.scluster_draw";

   Option_Period        : constant Option_Attr := "period";
   Default_Period       : constant Duration    := 0.1;

   Requires_Team           : aliased constant Internal_Key := "team";
   --  Assignment.object;
   Requires_Tasks          : aliased constant Internal_Key := "tasks";
   Requires_Real_Links     : aliased constant Internal_Key := "real_links";
   --  Connectivity_Matrix
   Requires_Scluster_Links : aliased constant Internal_Key := "scluster_links";

   Requires_All         : constant Internal_Key_Array :=
                            (Requires_Team'Access,
                             Requires_Tasks'Access,
                             Requires_Real_Links'Access,
                             Requires_Scluster_Links'Access);

   Provides_Queue_Mesh  : constant Internal_Key := "queue_mesh";
   Provides_Queue_Tasks : constant Internal_Key := "queue_tasks";
   --  Separate names in order to selectively disable

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node)
                    return      Sancta.Component.Object_Access;

   type Object is new Root.Object with record
      Period         : Duration := Default_Period;

      Busy           : Boolean := False; pragma Atomic (Busy);
   end record;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   not overriding
   procedure Do_Draw (This : in out Object);

end Sancta.Ctree.Component.Scluster_Draw;
