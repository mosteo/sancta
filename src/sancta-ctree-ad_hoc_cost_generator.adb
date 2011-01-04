with Sancta.Tasks.Explore_Directed_Segment;

with Agpl.Trace; use Agpl.Trace;

package body Sancta.Ctree.Ad_Hoc_Cost_Generator is

   ------------
   -- Create --
   ------------

   function Create
     (At_Pose : Types.Pose)
      return Object
   is
   begin
      return (Cost_Utils.Generator_Positioned.Euclidean.Create (At_Pose)
                with null record);
   end Create;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This : Object;
      Ini  : Sancta.Tasks.Object'Class;
      Fin  : Sancta.Tasks.Object'Class)
      return Sancta.Costs
   is
      package Explore renames Sancta.Tasks.Explore_Directed_Segment;
      Parent : Cost_Utils.Generator_Positioned.Euclidean.Object renames
        Cost_Utils.Generator_Positioned.Euclidean.Object (This);
   begin
      if Fin in Explore.Object'Class then
         Log ("Evaluating:" & Explore.Object (Fin).Image, Debug, Log_Section);
         return
           Costs'Min (Parent.Get_Cost (Ini, Fin),
                      Parent.Get_Cost (Ini, Explore.Object (Fin).Flip));
      else
         return Parent.Get_Cost (Ini, Fin);
      end if;
   end Get_Cost;

end Sancta.Ctree.Ad_Hoc_Cost_Generator;
