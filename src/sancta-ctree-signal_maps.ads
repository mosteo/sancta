private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Ordered_Sets;

with Agpl.Drawing;

with Sancta.Map;
with Sancta.Map.Smart;
with Sancta.Types;

package Sancta.Ctree.Signal_Maps is

   Log_Section : constant String := "sancta.ctree.signal_maps";

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

   type Density_View is new Agpl.Drawing.Drawable with private;

   function Create (From : Map_Family'Class) return Density_View;

   type Quality_View is new Agpl.Drawing.Drawable with private;

   function Create (From : Map_Family'Class) return Quality_View;

   procedure Set_Reference_Location (This : in out Quality_View;
                                     Loc  :        Map.Location'Class);
   --  Quality map as seen from this location static relay

private

   procedure Draw (This :        Density_View;
                   D    : in out Agpl.Drawing.Drawer'Class);

   procedure Draw (This :        Quality_View;
                   D    : in out Agpl.Drawing.Drawer'Class);

   package Loc_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Map.Location'Class, Map.Less_Than, Map."=");

   type Location_Pair is new Loc_Sets.Set with null record;

   function Image (Pair : Location_Pair) return String;

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

   protected type Map_Family_Safe is

      procedure Add_Observation (Pos_1 :        Types.Point;
                                 Pos_2 :        Types.Point;
                                 Q     :        Signal_Q);

      procedure Create (Over : Map.Smart.Object);

   private

      The_Map : Sancta.Map.Smart.Object;

      Count   : Location_Count_Maps.Map;
      Samples : Location_Samples.Map;
      --  Indexed by any of the two involved locations.
      --  This is needed for later quick drawing.

      Most_Sampled_Loc  : Natural := 0;
      Most_Sampled_Pair : Natural := 0;
      --  Cached values for quick access

   end Map_Family_Safe;

   type Map_Family is tagged limited record
      Safe : Map_Family_Safe;
   end record;

   type Map_Family_Access is access Map_Family;

   type Density_View is new Agpl.Drawing.Drawable with record
      Parent : Map_Family_Access;
   end record;

   type Quality_View is new Agpl.Drawing.Drawable with record
      Parent : Map_Family_Access;
   end record;

end Sancta.Ctree.Signal_Maps;
