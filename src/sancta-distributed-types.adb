with Sancta.Debug2;

with Agpl.Conversions; use Agpl.Conversions;
with Agpl.If_Function;
with Sancta.Plan_Node;
with Agpl; use Agpl;

package body Sancta.Distributed.Types is

   function Iif is new If_Function (String);
   use type Sancta.Costs;

   -----------
   -- Image --
   -----------

   function Image (This : in Danneal) return String is
   begin
      return
      "Found by: " & Image (This.Found_By) &
      "; Plan: " & Image (Dplan'(Object_Data with This.Plan)) &
      "; Ass: " & Image (Dassignment'(Object_Data with This.Assignment,
                                                       This.Ass_Cost));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (This : in Dassignment) return String is
      A : constant Sancta.Agent.Containers.Lists.List := This.Assignment.Get_Agents;
      R :          Ustring :=
            + ("Cost: " &
              Iif (This.Cost = Sancta.Infinite,
                   "Inf", To_String (Float (This.Cost))));

      use Asu;
      use Sancta.Agent.Containers.Lists;

      procedure Add (I : Cursor) is
      begin
         if R /= Null_Ustring then
            Append (R, "; ");
         end if;
         Append (R, Element (I).Get_Name & ":" &
                 Element (I).Get_Tasks.Length'Img & " T");
      end Add;

   begin
      A.Iterate (Add'Access);
      return +R;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (This : in Dcost_Matrix) return String is
      pragma Unreferenced (This);
   begin
      return "A cost matrix of unknown content";
   end Image;

   -----------
   -- Image --
   -----------

   function Image (This : in Dplan) return String is
   begin
      if This.Plan.Is_Empty then
         return "Empty";
      else
         return
         "Root id:" & Sancta.Plan_Node.Get_Id (This.Plan.Get_Root) &
         "; Total:" & This.Plan.Enumerate_Tasks (True, True, True, True).Length'Img &
         "; Primt:" & This.Plan.Enumerate_Tasks (False, True, True, True).Length'Img &
         "; Finis:" & This.Plan.Enumerate_Tasks (False, True, True, False).Length'Img;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (This : in Dstring_Set) return String is
      Result : Ustring;
      use Asu;
      use Agpl.Containers.String_Sets;

      procedure Add (I : Cursor) is
      begin
         if Result /= Null_Ustring then
            Append (Result, "; ");
         end if;
         Append (Result, Element (I));
      end Add;

   begin
      This.Set.Iterate (Add'Access);
      return +Result;
   end Image;

   function Image (This : in Dagent_Sequences) return String is
      Result : Ustring;
      use Agpl.Containers.String_Integer_Maps;
      use Asu;

      ---------
      -- Add --
      ---------

      procedure Add (I : Cursor) is
      begin
         if Result /= Null_Ustring then
            Append (Result, "; ");
         end if;
         Append (Result, Key (I) & ":");
         Append (Result, Element (I)'Img);
      end Add;

   begin
      This.Sequences.Iterate (Add'Access);
      return +Result;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (This : in Dstring) return String is
   begin
      return +This.Str;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (This : in String) return Dstring is
   begin
      return (Object_Data with +This);
   end Value;

   -----------
   -- Image --
   -----------

   function Image (This : in Dposition2d) return String is
      use Debug2;
   begin
      return "Pose: " & To_String (This.Position) &
             "; Vel: " & To_String (This.Velocity);
   end Image;

   ------------
   -- DReals --
   ------------

   package body DReals is

      -----------
      -- Build --
      -----------

      function Build (This : in Real)  return Dreal is
      begin
         return (Object_Data with This);
      end Build;

      -----------
      -- Value --
      -----------

      function Value (This : in Dreal) return Real is
      begin
         return This.R;
      end Value;

      -----------
      -- Image --
      -----------

      function Image (This : in Dreal) return String is
      begin
--         return This.R'Img;
         return To_String (Long_Long_Float (This.R));
      end Image;

   end DReals;

end Sancta.Distributed.Types;
