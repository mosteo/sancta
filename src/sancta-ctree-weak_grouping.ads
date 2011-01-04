with Sancta.Ctree.Connectivity_Matrix;

with Sancta.Agent.Containers;
with Sancta.Assignment;
with Sancta.Containers; use Sancta.Containers;

private with Ada.Containers.Indefinite_Vectors;

package Sancta.Ctree.Weak_Grouping is

   --     pragma Preelaborate;

   Log_Section : constant String := "nerus.weak_grouping";

   type Object (<>) is tagged private;

   function Create (A : Sancta.Assignment.Object'class;
                    L : Connectivity_Matrix.Object'Class) return Object;
   --  non-weakly connected robots generate groups of one robot.

   function Create (A : Sancta.Agent.Containers.Lists.List;
                    L : Connectivity_Matrix.Object'Class) return Object;

   function Get_Cheapest_Leaders (This : Object)
                                  return Sancta.Agent.Containers.Lists.List;
   --  Get a leader from each group, being it the one with cheapest first task.
   --  (Do not considers idle robots within a group!)

   function Get_Idle_Groups (This : Object) return Object;
   --  Return a new weak grouping that only includes groups totally idle

   function Get_Agents (This : Object) return Ac.Lists.List;
   --  List of agents, without grouping

   function Get_Mates (This : Object;
                       Bot  : String) return Sancta.Agent.Containers.Lists.List;
   --  Includes at least itself
   --  Empty list means no presence of given bot.

   function Get_Mates (This : Object;
                       Bot  : String) return Sancta.Assignment.Object;
   --  Will raise if not found.

   function Get_Group_Size (This : Object;
                            Bot  : String) return Natural;
   --  0 if robot is missing.
   --  cc. the number of weakly robots linked to it (plus itself).

   function Num_Groups (This : Object) return Natural;

   function Get_Group (This : Object;
                       Pos  : Positive) return Sancta.Assignment.Object;
   --  Pos must be in range 1 .. Num_Groups

   procedure Merge_With_Closest (This      :        Object;
                                 Bot       :        String;
                                 From      :        Sancta.Assignment.Object;
                                 New_Links : in out Connectivity_Matrix.Object'Class);
   --  Adds a link between the given bot and the closest one in another
   --  group, that must exist in From

   procedure Print (This : Object);

private

   use Ada.Containers;

   package Ac renames Sancta.Agent.Containers;
   package Avectors is
     new Indefinite_Vectors (Positive,
                             Sancta.Assignment.Object,
                             Sancta.Assignment."=");

   type Object is tagged record
      Links  : Connectivity_Matrix.Object;
      Agents : Ac.Maps.Map;
      Groups : Avectors.Vector;
   end record;

end Sancta.Ctree.Weak_Grouping;
