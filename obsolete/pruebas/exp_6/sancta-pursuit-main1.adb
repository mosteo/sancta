with Sancta.Assigners.Greedy;
with Sancta.Gui.Canvas;
with Sancta.Gui.Log;
with Sancta.Gui.Main;
with Sancta.Gui.Plan;
with Sancta.Gui.Robot;
with Sancta.Methods.Pursuit;
with Sancta.Robot;
with Sancta.Sections;
with Sancta.Tasks.Pursuit;
with Sancta.Types;

with Agpl.Constants;
with Agpl.Cr.Assigner.Hungry;
with Agpl.Cr.Assigner.Hungry2;
with Agpl.Cr.Controller;
with Agpl.Debug;
with Agpl.Htn.Plan;
with Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;
use  Agpl;

with Gtk.Widget; use Gtk.Widget;

with Ada.Calendar; use Ada.Calendar;
with Text_Io;      use Text_Io;

--  This file should be used with horca.* running in Player.
--  Intended to test the first approach to global pursuit. Completely unoptimi-
--  zed, when a gap is seen, someone else is sent to it, regardless of being
--  just a single gap. No approximation, etc.

procedure Sancta.Pursuit.Main1 is
   use type Sancta.Types.Real;

   Hungry     : Assigners.Greedy.Object;
   Hungry2    : Agpl.Cr.Assigner.Hungry2.Object;
   Hungry3    : Agpl.Cr.Assigner.Hungry.Object;
   Agents     : array (1 .. 4) of Robot.Object;
   Guis       : array (1 .. 4) of Gui.Robot.Object;
   Names      : constant array (Agents'Range) of String (1 .. 3) :=
                  ("Ari", "Ben", "Ced", "Dan");
   Addresses  : constant Ustring_Array (Agents'Range) :=
                  (others => U ("localhost"));

   Planed     : Boolean := false;
   Now        : Ada.Calendar.Time := Ada.Calendar.Clock;

   Plan       : Htn.Plan.Object;

   LAST_AGENT : constant := Agents'Last - 0;

   Tracer    : aliased Sancta.Gui.Log.Object;

   Step       : constant Duration := 0.1;
begin

   Gui.Main.Start;
   Put_Line ("Gui started.");

   Sancta.Gui.Log.Create (Tracer, Agpl.Trace.Debug, Console_Echo => true);
   Agpl.Trace.Set_Default_Tracer (Tracer'Unchecked_Access);

   --  Debugging sections:
   Gui.Log.Enable_Section (Tracer, Sections.Planning);
   Gui.Log.Enable_Section (Tracer, Agpl.Constants.CR);
   Gui.Log.Enable_Section (Tracer, Agpl.Constants.CV);
   Gui.Log.Enable_Section (Tracer, Agpl.Constants.HTN);

   declare
      Target     : Tasks.Pursuit.Object :=
                     Tasks.Pursuit.Create ((0.0, -5.0, 0.0), Wait => true);
      Purs       : Methods.Pursuit.Object;
      Controller : aliased Cr.Controller.Object;
      Gui_Plan   : aliased Gui.Plan.Object;
      Gui_Sync   : Gui.Plan.Synch (Gui_Plan'Access);
      W          : Gtk_Widget;
   begin
      Target.Get_Widget (W);
      Gui.Main.Add_Canvas (Gui.Canvas.Create ("Pursuit", W));

      Gui.Main.Add_Canvas
        (Gui.Canvas.Create ("Plan", Gui.Plan.Get_Widget (Gui_Plan)));

      Gui.Main.Add_Canvas
        (Gui.Canvas.Create ("Log", Gui.Log.Get_Widget (Tracer)));

      Controller.Add (Gui_Sync);

      Target.Set_Working_Area ((-10.0, -8.0, 0.0), (10.0, 8.0, 0.0));

      for I in Agents'First .. LAST_AGENT loop
         Agents (I) := Robot.Create (Names (I));
         Robot.Connect (Agents (I), S (Addresses (I)), 6664 + I);

         Gui.Robot.Create (Guis (I), Agents (I));
         Gui.Main.Add_Canvas
           (Gui.Canvas.Create (Names (I), Gui.Robot.Get_Widget (Guis (I))));

         Controller.Add (Agents (I));
      end loop;

      --  Plan expander
      Agpl.Htn.Plan.Add_Method (Plan, Purs);

      --  Assigners to try
      Controller.Add (Hungry);
      Controller.Add (Hungry2);
      Controller.Add (Hungry3);

      --  Objectives
      Htn.Plan.Add_Task (Plan, Target);

      while not Gui.Main.Shutdown loop
         Controller.Run;
         --  Controller.Print_Report;

         --  Pass plans once everyone is localized.
         if not Planed then
            Planed := true;
            for I in Agents'First .. LAST_AGENT loop
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

         Gui.Main.Update;

         delay until Now + Step;
         Now := Now + Step;
      end loop;

   end;
exception
   when E : others =>
      Put_Line ("Exception in main loop: " & Agpl.Debug.Report (E));
end Sancta.Pursuit.Main1;
