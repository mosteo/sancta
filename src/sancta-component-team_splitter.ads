with Sancta.Component.Root;

package Sancta.Component.Team_Splitter is

   --  Takes a team and outputs pose/velo goals, according to agent first task:
   --  No_task         : stop velocity
   --  Positioned task : pose goal
   --  Speed_Driving   : velo goal
   --  others          : exception

   pragma Beware ("This requires robots to be alphabetically ordered to work");

   Log_Section : constant String := "sancta.component.team_splitter";

   Name : aliased constant Component_Name := "team_splitter";

   Option_Amount  : constant Option_Attr  := "amount";
   --  Natural : agents in team.

   Requires_Team  : constant Internal_Key := "team";

   Provides_Pose_Goal : constant Internal_Key := "pose_goal";
   Provides_Velo_Goal : constant Internal_Key := "velo_goal";
   --  These two are prefixes; the agent number will be appended

   procedure Register;

private

   type Object is new Root.Object with null record;
   type Object_Access is access all Object'Class;

   function Create (Config : Agpl.Xml.Node)
                    return   Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Team_Splitter;
