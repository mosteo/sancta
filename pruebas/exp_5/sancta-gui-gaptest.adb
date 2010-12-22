with Sancta.Assigners.Greedy;
with Sancta.Gui.Canvas;
with Sancta.Gui.Main;
with Sancta.Robot;
with Sancta.Tasks.Wander_For_Gaps;
with Sancta.Types;

with Agpl.Cr.Controller;
with Agpl.Debug;
with Agpl.Htn.Plan;
with Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;
use  Agpl;

with Gtk.Widget; use Gtk.Widget;

with Ada.Calendar; use Ada.Calendar;
with Text_Io;      use Text_Io;

procedure Sancta.Gui.Gaptest is
   use type Sancta.Types.Real;

   Controller : aliased Cr.Controller.Object;
   Hungry     : Assigners.Greedy.Object;
   Agents     : array (1 .. 4) of Robot.Object;
   Names      : constant array (Agents'Range) of String (1 .. 3) :=
                  ("Ari", "Ben", "Ced", "Dan");
   Addresses  : constant Ustring_Array (Agents'Range) :=
                  (others => U ("localhost"));

   Tracer     : aliased Agpl.Trace.Object;
   Planed     : Boolean := false;
   Now        : Ada.Calendar.Time := Ada.Calendar.Clock;
   Step       : constant Duration := 0.01;

   Plan       : Htn.Plan.Object;
begin

   Gui.Main.Start;
   Put_Line ("Gui started.");

   Agpl.Trace.Create (Tracer, Agpl.Trace.Debug, true);
   Agpl.Trace.Set_Default_Tracer (Tracer'Unchecked_Access);

   declare
      Target     : Tasks.Wander_For_Gaps.Object;
      W          : Gtk_Widget;
   begin
      Tasks.Wander_For_Gaps.Create_Widget (Target, W);
      Gui.Main.Add_Canvas (Gui.Canvas.Create ("Gaps", W));

      Target.Speed     := 0.0;
      Target.Rot_Speed := 0.0;
      Target.Safe_Side := 0.8;
      Target.Reload    := 0.1;

      for I in 1 .. 1 loop
         Agents (I) := Robot.Create (Names (I));
         Robot.Connect (Agents (I), S (Addresses (I)), 6664 + I);
         Controller.Add (Agents (I));
      end loop;

      --  Assigners to try
      Controller.Add (Hungry);

      --  Objectives
      Htn.Plan.Add_Task (Plan, Target);

   end;

   while not Gui.Main.Shutdown loop
      Controller.Run;
      --  Controller.Print_Report;

      --  Pass plans once everyone is localized.
      if not Planed then
         Planed := true;
         for I in 1 .. 1 loop
            Planed := Planed and Robot.Localized (Agents (I));
            exit when not Planed;
         end loop;
         if Planed then
            --  Pass to the controller
            Put_Line ("Sending plans...");
            Controller.Add (Plan);
            Put_Line ("Plans issued.");
         end if;
      end if;

      delay until Now + Step;
      Now := Now + Step;

      --  Gui updating:
      Gui.Main.Update;
   end loop;
exception
   when E : others =>
      Put_Line ("Exception in main loop: " & Agpl.Debug.Report (E));
end Sancta.Gui.Gaptest;
