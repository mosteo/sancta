--  Sample empty component to save-as when creating new ones.

with Ada.Calendar,
     Agpl.Tasking.Period,
     Sancta.Assignment,
     Sancta.Component,
     Sancta.Component.Root;

use Sancta,
    Sancta.Component;

package Sancta.Ctree.Component.Springed_Team_vRAS is

   --  Moves one robot of the springed team, using the computed velo

   Log_Section : constant String := "Sancta.Ctree.Component.springed_team_vras";

   Name : aliased constant Component_Name := "springed_team_vras";

   Requires_Team      : constant Internal_Key := "team";
   Provides_Velo_Goal : constant Internal_Key := "velo_goal";

   Option_Agent_Name  : constant Option_Attr  := "agent_name";
   --  The agent whose velocity will be output

   Option_R  : constant Option_Attr := "r";  -- Spring rest length (link range)
   Option_K  : constant Option_Attr := "k";  -- Spring strength
   Option_Gv : constant Option_Attr := "gv"; -- Goal velocity
   Option_Kv : constant Option_Attr := "kv"; -- Spring pulling velocity
   Option_F  : constant Option_Attr := "f";  -- Rozamiento

   --  The defaults used for RAS:
   Default_K  : constant Float := 2.0;
   Default_Gv : constant Float := 1.5;
   Default_Kv : constant Float := 3.0;
   Default_F  : constant Float := 3.0;

   Option_Period  : constant Option_Attr := "period";
   Default_Period : constant Duration    := 0.1;
   --  Update period

   procedure Register;

private

   use Agpl;

   type Object is new Root.Object with record
      R  : Float;
      K  : Float := Default_K;
      Gv : Float := Default_Gv;
      Kv : Float := Default_Kv;
      F  : Float := Default_F;

      Period : Tasking.Period.Object := Tasking.Period.Create (Default_Period);
   end record;

   function Create (Config : Comp_Config)
                    return   Component.Object_Access;

--     overriding
--     procedure Key_Stored (This  : in out Object;
--                           Key   : in     Internal_Key;
--                           Value : in     Data'Class);

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   not overriding
   procedure Perform (This : Object;
                      Team : Assignment.Object);

end Sancta.Ctree.Component.Springed_Team_vRAS;
