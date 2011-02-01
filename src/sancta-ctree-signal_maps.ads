with Sancta.Map;
with Sancta.Map.Qtree;

package Sancta.Ctree.Signal_Maps is

   type Map_Family is tagged limited private;
   --  This type holds all observations made by a single robot, of quality in
   --  regard to other robots.
   --  It can be queried to ascertain how many observations were made from some
   --    location.
   --  Once a reference location is set, the observations to/fro this reference
   --    location become available.

   procedure Add_Observation (This  : in out Map_Family;
                              Pos_1 :        Types.Pose;
                              Pos_2 :        Types.Pose;
                              Q     :        Signal_Q);

private

   type Map_Family is tagged limited null record;

end Sancta.Ctree.Signal_Maps;
