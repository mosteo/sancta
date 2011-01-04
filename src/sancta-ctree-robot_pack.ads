with Sancta.Ctree.Robot;

with Sancta.Types;
use  Sancta;

with Sancta; with Sancta.Criteria; use Sancta.Criteria;
with Sancta.Agent.Containers;
with Sancta.Tasks;
with Agpl.Ustrings; use Agpl.Ustrings;
 

package Sancta.Ctree.Robot_Pack is

   package Ac renames Sancta.Agent.Containers;

   --  Used as an aggregator of agents in nerus-assigner-complex
   --  The external view is as an agent.
   --  Costs are given by the nearest member to a task
   --  Pose is the centroid of all robots
   --  Tasks assigned to this agent go to all agents.

   --  Note that packs of packs could be possible with this arrangement, but this should
   --  not happend in the current Complex algorithm, so there are checks for ensuring
   --  proper classes herein.

   type Object is new Sancta.Ctree.Robot.Object with private;

   not overriding
   function Create (From : Robot.Object) return Object;

   not overriding
   function Create (From : Ac.Lists.List) return Object;
   --  A pack formed by those agents.

   not overriding
   procedure Create (This :    out Object;
                     Goal :        Sancta.Types.Pose;
                     From : in out Ac.Lists.List;
                     Size :        Positive := 1);
   --  Create a robot pack, chosing Size closest ones to the given goal.
   --  The agents in From may have Positioned tasks that will be used to check the
   --  last task pose.

   not overriding
   function Get_Agents (This : Object) return Sancta.Agent.Containers.Lists.List;
   --  Get the agents forming this pack, each with a copy of the tasks

   overriding
   function Get_Cost (This : in Object;
                      From,
                      To   : in Sancta.Tasks.Object'Class)
                      return Sancta.Costs;
   --  Returns the cost of the nearest member

   overriding
   function Get_Name (This : in Object) return String;
   --  Concatenates the names of all members, alphabetically.

   overriding
   function Get_Pose (This : Object) return Types.Pose;
   --  Returns center of mass

private

   type Object is new Sancta.Ctree.Robot.Object with record
      Name : Ustring;

      Bots : Ac.Maps.Map;
   end record;

end Sancta.Ctree.Robot_Pack;
