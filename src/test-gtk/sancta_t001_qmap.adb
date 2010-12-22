with Ada.Command_Line; use Ada.Command_Line;

with Agpl.Chronos;
with Agpl.Drawing.Buffer;
with Agpl.Gdk.Managed.Drawing_Area;
with Agpl.Png;
with Agpl.Trace; use Agpl.Trace;

with Sancta.Map.Qtree;
with Sancta.Map.Qtree.Show;
with Sancta.Map.Utils;
with Sancta.Types; use Sancta.Types;

procedure Sancta_T001_Qmap is
   M : Sancta.Map.Qtree.Object (Sancta.Map.Qtree.Vicinity_6);
   P : Agpl.Png.Png_File;

   Max_Depth  : Natural;
   Load_Timer : Agpl.Chronos.Object;
   Draw       : Agpl.Drawing.Buffer.Object;
begin
   if Argument_Count /= 2 then
      Log ("program <max depth> <file.png>", Error);
      return;
   end if;

   Set_Level (Debug);
   Enable_Section (Agpl.Png.Log_Section);
   --   Enable_Section (Sancta.Map.Qtree.Det_Section);
   Enable_Section (Sancta.Map.Qtree.Log_Section);

   P.Open (Argument (2));
   Max_Depth := 2 ** Natural'Value (Argument (1));

   M.Set_Size      (0.0, X_Real (P.Width), 0.0, Y_Real (P.Height));

   M.Set_Cell_Size (X_Real (P.Width) / X_Real (Max_Depth),
                    X_Real (P.Width),
                    Y_Real (P.Height) / Y_Real (Max_Depth),
                    Y_Real (P.Height));

   M.From_Png (Argument (2));
   Log ("Loading time: " & Load_Timer.Image, Always);

   --  Random path in map to test path-finding
   Log ("Finding path...", Always);
   begin
      declare
         function Valid (O : Sancta.Map.Observation'Class) return Boolean is
            use Sancta.Map.Qtree;
         begin
            return Observation (O).Terrain = Free;
         end Valid;
         Ini : constant Sancta.Map.Location'Class :=
                 Sancta.Map.Utils.Random_Location (M, Valid'Access);
         Fin : constant Sancta.Map.Location'Class :=
                 Sancta.Map.Utils.Random_Location (M, Valid'Access);

         Way_Timer : Agpl.Chronos.Object;
         Way : constant Sancta.Map.Path_With_Cost := M.Best_Path (Ini, Fin);
      begin
         --  Log ("Path: " & Sancta.Map.Image (Way.Path), Always);
         Log ("Path-finding time: " & Way_Timer.Image, Always);

         Log ("Showing", Always);
         Sancta.Map.Qtree.Show.Draw (M, Way.Path, Draw);

         --     Log ("Printing", Always);
         --     M.Print;
      end;
   exception
      when Constraint_Error =>
         --  No locations, show it without path
         Log ("Showing", Always);
         Sancta.Map.Qtree.Show.Draw (M, D => Draw);
   end;

   Agpl.Gdk.Managed.Drawing_Area.Show (Draw, "QMap");

   Log ("The end", Always);

   delay 1_000_000.0; -- Avoid losing M scope.
exception
   when E : others =>
      Log ("Main: " & Report (E), Error);
end Sancta_T001_Qmap;
