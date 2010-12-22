with Agpl.Tasking.Period,
     Sancta.Assignment,
     Sancta.Component.Root;

package Sancta.Component.Team is

   --  Takes a pose and vel and provides a unifying agent.

   Name : aliased constant Component_Name := "team";

   Option_Amount  : constant Option_Attr  := "amount";
   --  Natural : agents in team.

   Option_Period  : constant Option_Attr := "period";
   Default_Period : constant Duration := 0.1;

   Requires_Agent  : constant Internal_Key := "agent";
   --  Prefix for agent inputs; the matching outputs should have an index
   --    appended: agent1, agent2, etc

   Provides_Team : constant Internal_Key := "team";
   --  Of class Types.Team

   procedure Register;

private

   use Agpl;

   type Object is new Root.Object with record
      Team   : Assignment.Object;
      Period : Tasking.Period.Object := Tasking.Period.Create (Default_Period);
   end record;

   type Object_Access is access all Object'Class;

   function Create (Config : Agpl.Xml.Node)
                    return   Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);
   --  Team data is locally updated reacting to this

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);
   --  The whole team is published periodically

end Sancta.Component.Team;
