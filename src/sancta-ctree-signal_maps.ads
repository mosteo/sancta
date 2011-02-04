private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Ordered_Sets;

with Sancta.Map;
with Sancta.Map.Smart;
with Sancta.Types;

package Sancta.Ctree.Signal_Maps is

   type Map_Family is tagged limited private;
   --  This type holds all observations made by a single robot, of quality in
   --  regard to other robots.
   --  It can be queried to ascertain how many observations were made from some
   --    location.
   --  Once a reference location is set, the observations to/fro this reference
   --    location become available.

   procedure Create (This : in out Map_Family;
                     Over :        Sancta.Map.Smart.Object);

   procedure Add_Observation (This  : in out Map_Family;
                              Pos_1 :        Types.Point;
                              Pos_2 :        Types.Point;
                              Q     :        Signal_Q);

private

   package Loc_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Map.Location'Class, Map.Less_Than, Map."=");

   type Location_Pair is new Loc_Sets.Set with null record;

   function Pair (L, R : Map.Location'Class) return Location_Pair;
   pragma Postcondition (Natural (Pair'Result.Length) = 2);
   pragma Inline (Pair);

--     function "<" (L, R : Location_Pair) return Boolean;
--     pragma Inline ("<");

   package Location_Count_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Map.Location'Class, Natural, Map.Less_Than);

   package Q_Lists is new Ada.Containers.Doubly_Linked_Lists (Signal_Q);

   type Pair_Sample is record
      Pair    : Location_Pair;
      Samples : Q_Lists.List;
   end record;

   type Pair_Sample_Access is access Pair_Sample;

   package Pair_Samples is new Ada.Containers.Indefinite_Ordered_Maps
     (Map.Location'Class, Pair_Sample_Access, Map.Less_Than);
   type Pair_Samples_Map_Access is access Pair_Samples.Map;
   package Location_Samples is new Ada.Containers.Indefinite_Ordered_Maps
     (Map.Location'Class, Pair_Samples_Map_Access, Map.Less_Than);
   --  Together, we have a Loc that points to another Loc which points to a
   --    proper collection of samples for both.
   --  The symmetric route must give the same Pair_Samples object!
   --  We use a Ptr to List in order to save copies.
   --  Since this should live all the way to program exit,
   --    I'll not worry about leaks here.

   type Map_Family is tagged limited record
      Map   : Sancta.Map.Smart.Object;

      Count   : Location_Count_Maps.Map;
      Samples : Location_Samples.Map;
      --  Indexed by any of the two involved locations.
      --  This is needed for later quick drawing.
   end record;

end Sancta.Ctree.Signal_Maps;
