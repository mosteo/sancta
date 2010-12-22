--  Algorithm to compute paths in a grid. NC-1 I think.

with Sancta.Map.Build_Wavefront_Route;
with Sancta.Map.Create_Wavefront;

with Agpl.Text_Io; use Agpl.Text_Io;

--------------------------
-- Sancta.Map.Wavefront --
--------------------------

function Sancta.Map.Wavefront
  (From,
   To    : Location'Class;
   Map   : Sancta.Map.Object'Class)
   return  Location_Lists.List
is

   function Create is new Create_Wavefront (Neighbors, Get_Cost);
   function Build  is new Build_Wavefront_Route (Neighbors);

begin
   --  Put_Line ("Visited cells:" & Create (To, Map, From).Length'Img);
   return Build (From, To,
                 Create (To, Map, Sancta.Map.Location_Handle.Set (From)), Map);
end Sancta.Map.Wavefront;
