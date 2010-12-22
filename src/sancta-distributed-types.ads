with Sancta.Types;

with Agpl.Containers.String_Integer_Maps;
with Agpl.Containers.String_Sets;
with Sancta;
with Sancta.Agent.Containers;
with Sancta.Assignment;
with Sancta.Cost_Cache.Handle;
with Sancta.Cost_Matrix;
with Sancta.Criteria;
with Sancta.Plan;

package Sancta.Distributed.Types is

   --  Pragma preelaborate;

   type Danneal is new Object_Data with record
      Plan        : Sancta.Plan.Object;

      Assignment  : Sancta.Assignment.Object;         -- Ass corresponding to Plan
      Ass_Cost    : Sancta.Costs := Sancta.Infinite; -- And its cost
      Criterion   : Criteria.Assignment_Criteria := Criteria.Criterion_Invalid;
      --  The criterion used for the assignment

      History     : Sancta.Assignment.Object;
      --  Recording of what each agent did.

      Agent_Costs : Sancta.Cost_Cache.Handle.Object;
      --  A cost cache, preferibly lightweighted.

      Found_By    : Node_Id;
      Agents      : Sancta.Agent.Containers.Maps.Map;

      Proceed     : Boolean := False;
      --  if not Proceed, tasks will not be assigned so execution is delayed.
   end record;
   function Image (This : in Danneal) return String;

   type Dassignment is new Object_Data with record
      Assignment : Sancta.Assignment.Object;
      Cost       : Sancta.Costs;
   end record;
   function Image (This : in Dassignment) return String;

   type Dstring is new Object_Data with private;
   function Value (This : in String) return Dstring;
   function Image (This : in Dstring) return String;

   generic
      type Real is digits <>;
   package DReals is
      type Dreal is new Object_Data with private;
      function Build (This : in Real)  return Dreal;
      function Value (This : in Dreal) return Real;
      function Image (This : in Dreal) return String;

   private
      type Dreal is new Object_Data with record
         R : Real;
      end record;
   end DReals;

   type Dcost_Matrix is new Object_Data with record
      Cost_Matrix : Sancta.Cost_Matrix.Object;
   end record;
   function Image (This : in Dcost_Matrix) return String;

   type Dplan is new Object_Data with record
      Plan : Sancta.Plan.Object;
   end record;
   function Image (This : in Dplan) return String;

   type Dstring_Set is new Object_Data with record
      Set : Agpl.Containers.String_Sets.Set;
   end record;
   function Image (This : in Dstring_Set) return String;

   type Agent_Sequences is new Agpl.Containers.String_Integer_Maps.Map
     with null record;

   type Dagent_Sequences is new Object_Data with record
      Sequences : Agent_Sequences;
   end record;
   function Image (This : in Dagent_Sequences) return String;

   type Dposition2d is new Object_Data with record
      Position : Sancta.Types.Pose;
      Velocity : Sancta.Types.Pose;
   end record;
   function Image (This : in Dposition2d) return String;

private

   type Dstring is new Object_Data with record
      Str : Ustring;
   end record;

end Sancta.Distributed.Types;
