with Sancta.Types.Operations;

with Sancta.Agent.Handle;
with Sancta.Agent.Utils; use Sancta.Agent.Utils;

package body Sancta.Ctree.Robot_Pack is

   ----------------
   -- Build_Name --
   ----------------

   function Build_Name (Agents : Ac.Lists.List) return String is
      Result : Ustring;

      procedure Add (I : Ac.Lists.Cursor) is
      begin
         Asu.Append (Result, Ac.Lists.Element (I).Get_Name);
         Asu.Append (Result, "*");
      end Add;
   begin
      Agents.Iterate (Add'Access);
      return +Result;
   end Build_Name;

   ------------
   -- Create --
   ------------

   function Create
     (From : Robot.Object)
      return Object
   is
   begin
      return (Robot.Object with
              Name => +From.Get_Name,
              Bots => To_Map (From.Get_Name, From));
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (From : Ac.Lists.List)
      return Object
   is
      function Key (A : Sancta.Agent.Object'Class) return String is
      begin
         return A.Get_Name;
      end Key;
   begin
      return (Robot.Object with
              Name => +Build_Name (From),
              Bots => To_Map (From, Key'Access));
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (This :    out Object;
      Goal :        Sancta.Types.Pose;
      From : in out Ac.Lists.List;
      Size :        Positive := 1)
   is
      function Closest return Sancta.Agent.Object'Class is
         Best_Dist : Types.Real := Types.Real'Last;
         Best_Bot  : Sancta.Agent.Handle.Object;
         use Ac.Lists;
         procedure Check (I : Cursor) is
            use Types;
            use Operations;
            Bot : constant Robot.Object := Robot.Object (Element (I));
         begin
            if Distance (Bot.Get_Pose, Goal) < Best_Dist then
               Best_Dist := Distance (Bot.Get_Pose, Goal);
               Best_Bot.Set (Bot);
            end if;
         end Check;
      begin
         From.Iterate (Check'Access);
         return Best_Bot.Get;
      end Closest;

      Chosen : Ac.Lists.List;
   begin
      if Natural (From.Length) < Size then
         raise Constraint_Error with "Not enough agents for requested size";
      end if;

      for I in 1 .. Size loop
         declare
            Best : constant Sancta.Agent.Object'Class := Closest;
         begin
            Chosen.Append (Best);
            From := From - Best;
         end;
      end loop;

      This := Create (Chosen);
   end Create;

   ----------------
   -- Get_Agents --
   ----------------

   function Get_Agents
     (This : Object)
      return Sancta.Agent.Containers.Lists.List
   is
      Result : Ac.Lists.List;

      procedure Add (I : Ac.Maps.Cursor) is
         Agent : Sancta.Agent.Object'Class := Ac.Maps.Element (I);
      begin
         Agent.Set_Tasks (Robot.Object (This).Get_Tasks);
         Result.Append (Agent);
      end Add;
   begin
      This.Bots.Iterate (Add'Access);
      return Result;
   end Get_Agents;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This : in Object;
      From,
      To   : in Sancta.Tasks.Object'Class)
      return Sancta.Costs
   is
      Best : Sancta.Costs := Sancta.Infinite;
      procedure Check (I : Ac.Maps.Cursor) is
      begin
         Best := Sancta.Costs'Min
           (Best,
            Ac.Maps.Element (I).Get_Cost (From, To));
      end Check;
   begin
      This.Bots.Iterate (Check'Access);
      return Best;
   end Get_Cost;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (This : in Object)
      return String
   is
   begin
      return +This.Name;
   end Get_Name;

   --------------
   -- Get_Pose --
   --------------

   function Get_Pose (This : Object) return Types.Pose is
      Centroid : Types.Pose := Types.Origin;

      use Ac.Maps;
      use Types;
      procedure Add (I : Cursor) is
      begin
         Centroid := Centroid + Robot.Object (Element (I)).Get_Pose;
      end Add;
   begin
      This.Bots.Iterate (Add'Access);
      return Centroid / Types.Real (This.Bots.Length);
   end Get_Pose;

end Sancta.Ctree.Robot_Pack;
