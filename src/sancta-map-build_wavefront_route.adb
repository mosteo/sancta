--  Algorithm to compute paths in a grid. NC-1 I think.


--------------------------------------
-- Sancta.Map.Build_Wavefront_Route --
--------------------------------------

function Sancta.Map.Build_Wavefront_Route
  (From,
   To    : Location'Class;
   Wave  : Loc_Cost_Maps.Map;
   Map   : Sancta.Map.Object'Class)
   return  Location_Lists.List
is

   Visited   : Loc_Cost_Maps.Map renames Wave;
   Route     : Location_Lists.List;
   Safe      : Natural := 0;
   Best_Cost : Costs := Infinite;

begin

   Route.Append (From);

   loop
      Safe := Safe + 1;
      if Safe > 1000 then
         raise Program_Error
           with "S.Map.Wavefront_Route: No route could be found!?";
      end if;

      exit when Route.Last_Element = To;
      declare
         Near       : constant Location_Vectors.Vector :=
                        Neighbors (Map, Route.Last_Element);
         Best_Index : Positive := Positive'Last;
      begin
         for I in Near.First_Index .. Near.Last_Index loop
            if Visited.Contains (Near.Element (I)) and then
              Visited.Element (Near.Element (I)) < Best_Cost
            then
               Best_Index := I;
               Best_Cost  := Visited.Element (Near.Element (I));
            end if;
         end loop;
         --  Put_Line ("Appending " & Near.Element (Best_Index).Image);
         if Best_Index < Positive'Last then
            Route.Append (Near.Element (Best_Index));
         else
            raise Constraint_Error with "No route found?";
         end if;
      end;
   end loop;

   return Route;
end Sancta.Map.Build_Wavefront_Route;
