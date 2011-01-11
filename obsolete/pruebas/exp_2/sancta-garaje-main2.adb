--  Main executable.
--  The purpose of this test is to look for a blob in a map.

with Sancta.Assigners.Greedy;
with Sancta.Robot;
with Sancta.Methods.Panorama_Expansion;
with Sancta.Methods.Reticle_Expansion;
with Sancta.Tasks.Search_Blob_In;
with Sancta.Types;

with Agpl.Cr.Assigner.Hungry;
with Agpl.Cr.Assigner.Hungry2;
with Agpl.Cr.Controller;
with Agpl.Htn.Plan;
with Agpl.Trace;
with Agpl.Types.Ustrings;
use  Agpl.Types.Ustrings;
use  Agpl;

with Ada.Calendar; use Ada.Calendar;

--  Usar con garaje.cfg

procedure Sancta.Garaje.Main2 is

   use type Sancta.Types.Real;

   Controller : aliased Cr.Controller.Object;
   Now        : Time              := Clock;
   Step       : constant Duration := 0.01;
   Target     : constant Tasks.Search_Blob_In.Object :=
                  Tasks.Search_Blob_In.Create_Rectangle
                    (Left  => -10.0, Right  => 10.0,
                     Top   =>   8.0, Bottom => -8.0,
                     Color => (200, 100, 100));

   Plan       : Htn.Plan.Object;

   Pan_Expand : Methods.Panorama_Expansion.Object;
   Ret_Rect   : Methods.Reticle_Expansion.Object (Methods.Reticle_Expansion.Rect);
   Ret_Hex    : Methods.Reticle_Expansion.Object (Methods.Reticle_Expansion.Hex);

   Hungry     : Assigners.Greedy.Object;
   Hungry2    : Agpl.Cr.Assigner.Hungry2.Object;
   Hungry3    : Agpl.Cr.Assigner.Hungry.Object;

   Agents     : array (1 .. 4) of Robot.Object;
   Names      : constant array (Agents'Range) of String (1 .. 3) :=
                  ("Ari", "Ben", "Ced", "Dan");
   Addresses  : constant Ustring_Array (Agents'Range) :=
                  (others => U ("localhost"));

   Tracer     : aliased Agpl.Trace.Object;

   Planed     : Boolean := false;
begin
   Agpl.Trace.Create (Tracer, Agpl.Trace.Debug, true);
   Agpl.Trace.Set_Default_Tracer (Tracer'Unchecked_Access);

   for I in Agents'Range loop
      Agents (I) := Robot.Create (Names (I));
      Robot.Set_Delayed_Until (Agents (I), Now + 5.0 * Duration (I));
      Robot.Connect (Agents (I), S (Addresses (I)), 6664 + I);
      Controller.Add (Agents (I));
   end loop;

   --  Assigners to try
   Controller.Add (Hungry);
   Controller.Add (Hungry2);
   Controller.Add (Hungry3);

   --  Methods to apply
   Htn.Plan.Add_Method (Plan, Ret_Rect);
   Htn.Plan.Add_Method (Plan, Ret_Hex);
   Htn.Plan.Add_Method (Plan, Pan_Expand);

   --  Objectives
   Htn.Plan.Add_Task (Plan, Target);

   while true loop
      Controller.Run;
      --  Controller.Print_Report;

      --  Pass plans once everyone is localized.
      if not Planed then
         Planed := true;
         for I in Agents'Range loop
            Planed := Planed and Robot.Localized (Agents (I));
            exit when not Planed;
         end loop;
         if Planed then
            --  Pass to the controller
            Controller.Add (Plan);
         end if;
      end if;

      delay until Now + Step;
      Now := Now + Step;
   end loop;
end Sancta.Garaje.Main2;
