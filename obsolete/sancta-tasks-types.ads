--  Here we define types that would be used by tasks and the GUI.
--  Doing so we break the circular dependency that would happen when
--  creating callbacks.

--  with Sancta.Gui.User_Data_Handle;
with Sancta.Types;

with Sancta.Gap.Safe_Array;
with Sancta.Map;
with Agpl.Smart_Access;

--  with Gtk.Widget; use Gtk.Widget;

package Sancta.Tasks.Types is

   pragma Preelaborate;

--   subtype User_Data is Gui.User_Data_Handle.Object;

   --  Observation types for pursuit
   type Pursuit_Cell_States is (Clean, Contaminated, Obstacle);
   type Pursuit_Observations is new Sancta.Map.Observations with
      record
         Value : Pursuit_Cell_States;
      end record;

   type Cover_Gaps_And_Dispatch is
      record
         Pending : Natural := 0; -- Robots pending to be positioned.
         --  Used to indicate to the holding robot when he is free again.
      end record;

   type Cover_Access is access all Cover_Gaps_And_Dispatch;

   package Smart_Cover is new Agpl.Smart_Access
     (Cover_Gaps_And_Dispatch, Cover_Access);

   type Wander_For_Gaps_Info is new Sancta.Types.User_Data with record
      Gaps : Sancta.Gap.Safe_Array.Object;
   end record;

end Sancta.Tasks.Types;
