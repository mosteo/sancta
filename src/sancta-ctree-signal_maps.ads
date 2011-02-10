private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Ordered_Sets;

with Agpl.Drawing;
with Agpl.Gui;

with Sancta.Component;
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
                     Over :        Sancta.Map.Smart.Object;
                     Q_T  :        Signal_Q);
   --  Q_T: Quality_Threshold

   procedure Add_Observation (This  : in out Map_Family;
                              Pos_1 :        Types.Point;
                              Pos_2 :        Types.Point;
                              Q     :        Signal_Q);

   type Quality_View is new
     Component.Data and
     Agpl.Drawing.Drawable and
     Agpl.Gui.Event_Handler with private;
   --  This view of the type shows the gathered data.
   --  It has two modes: a first one, in which sample density is shown.
   --  A second one, in which quality towards a selected location is shown.

   function Create (From : Map_Family'Class) return Quality_View;

private

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

   protected type Map_Family_Safe (Parent : access Map_Family) is

      procedure Add_Observation (Pos_1 :        Types.Point;
                                 Pos_2 :        Types.Point;
                                 Q     :        Signal_Q);

      procedure Create (Over : Map.Smart.Object);

      procedure Clicked (P : Types.Point);
      procedure Draw_Decide  (Into : in out Agpl.Drawing.Drawer'Class);
      procedure Draw_Density (Into : in out Agpl.Drawing.Drawer'Class);
      procedure Draw_Quality (From :        Map.Location'Class;
                              Into : in out Agpl.Drawing.Drawer'Class);

   private

      The_Map : Sancta.Map.Smart.Object;

      Count   : Location_Count_Maps.Map;
      Samples : Location_Samples.Map;
      --  Indexed by any of the two involved locations.
      --  This is needed for later quick drawing.

      Most_Sampled_Loc  : Natural := 0;
      Most_Sampled_Pair : Natural := 0;
      --  Cached values for quick access

      Ref_Loc : Map.Location_Handle.Object;

   end Map_Family_Safe;

   type Map_Family_Access is access all Map_Family;

   type Map_Family is tagged limited record
      Self : Map_Family_Access := Map_Family'Unchecked_Access;
      Safe : Map_Family_Safe (Map_Family'Access);

      Threshold : Signal_Q;
   end record;

   type Quality_View is new
     Component.Data and
     Agpl.Drawing.Drawable and
     Agpl.Gui.Event_Handler with
      record
         Parent : Map_Family_Access;
      end record;

   overriding
   procedure Triggered (Handler : in out Quality_View; E : Agpl.Gui.Event'Class);

end Sancta.Ctree.Signal_Maps;
