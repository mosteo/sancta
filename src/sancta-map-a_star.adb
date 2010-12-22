with Agpl.Search.A_Star;
pragma Elaborate_All (Agpl.Search.A_Star);

package body Sancta.Map.A_Star is

   package Solver is
     new Agpl.Search.A_Star (Location_Handle.Object,
                             Costs,
                             0.0,
                             Image => Costs'Image);

   --------------
   -- Num_Next --
   --------------

   function Num_Next (L : Location_Handle.Object) return Natural is
   begin
      return L.Ref.Num_Neighbors;
   end Num_Next;

   ----------
   -- Next --
   ----------

   function Next (L : Location_Handle.Object;
                  I : Positive)
                  return Location_Handle.Object
   is
      N : constant Location'Class := L.Ref.Neighbor (I);
   begin
      return Location_Handle.Set (N);
   end Next;

   ----------
   -- Cost --
   ----------

   function Cost (Ini, Fin : Location_Handle.Object) return Costs is
   begin
      return Ini.Ref.Real_Cost (Fin.Ref.all);
   end Cost;

   --------------
   -- Estimate --
   --------------

   function Estimate (Ini, Fin : Location_Handle.Object) return Costs is
   begin
      return Ini.Ref.Estimate_Cost (Fin.Ref.all);
   end Estimate;

   -----------
   -- Image --
   -----------

   function Image (L : Location_Handle.Object) return String is
   begin
      return L.Ref.Image;
   end Image;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (P : in out Map.Path; L : Location_Handle.Object) is
   begin
      P.Prepend (L.Ref.all);
   end Prepend;

   procedure Best is new Solver.Best_Path (Num_Next,
                                           Next,
                                           Cost,
                                           Estimate,
                                           Image,
                                           Map.Path,
                                           Prepend);

   ---------------
   -- Best_Path --
   ---------------

   function Best_Path (Ini,
                       Fin : Location'Class) return Path_With_Cost
   is
      Result : Path_With_Cost;
   begin
      Best (Location_Handle.Set (Ini),
            Location_Handle.Set (Fin),
            Result.Path,
            Result.Cost);
      return Result;
   end Best_Path;

end Sancta.Map.A_Star;
