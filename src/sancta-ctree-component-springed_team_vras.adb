with Agpl.Interfaces.C.Types,
     Sancta.Agent,
     Sancta.Agent.Utils,
     Sancta.Component.Ctypes,
     Sancta.Component.Factory,
     Sancta.Containers,
     Sancta.Debug2,
     Sancta.Located_Agent,
     Sancta.Tasks.Positioned,
     Sancta.Types;

package body Sancta.Ctree.Component.Springed_Team_vRAS is

   package Ictypes renames Agpl.Interfaces.C.Types;

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Comp_Config)
      return Sancta.Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Verify (Option_Agent_Name);
      This.Verify (Option_R);
      This.R := Float'Value (This.Option (Option_R));

      if This.Exists (Option_K) then
         This.K := Float'Value (This.Option (Option_K));
      end if;

      if This.Exists (Option_Kv) then
         This.Kv := Float'Value (This.Option (Option_Kv));
      end if;

      if This.Exists (Option_Gv) then
         This.gv := Float'Value (This.Option (Option_gv));
      end if;

      if This.Exists (Option_F) then
         This.F := Float'Value (This.Option (Option_F));
      end if;

      if This.Exists (Option_Period) then
         This.Period.Set_Period (Duration'Value (This.Option (Option_Period)));
      end if;

      return Sancta.Component.Object_Access (This);
   end Create;

   -------------
   -- Perform --
   -------------

   procedure Perform (This : Object; Team : Assignment.Object)
   is
      use Sancta.Containers,
          Sancta.Agent.Utils;
      function By_Name (L, R : Agent.Object'Class) return Boolean is
      begin
         return L.Get_Name < R.Get_Name;
      end By_Name;

      package Sort is new Ac.Vectors.Generic_Sorting (By_Name);

      Bots : Ac.Vectors.Vector := To_Vector (Team.Get_Agents);
   begin
      --  Sort agents by name:
      Sort.Sort (Bots);
      --  Log ("Num bots in team:" & Bots.Length'Img, Debug, Log_Section);

      --  Begin with the C crap:
      declare
         function Index_Of (Name : String) return Ictypes.Int is
            use type Ictypes.Int;
            Idx : Ictypes.Int := 0;
         begin
            for I in Bots.First_Index .. Bots.Last_Index loop
               if Bots.Element (I).Get_Name = Name then
                  return Idx;
               else
                  Idx := Idx + 1;
               end if;
            end loop;
            raise Constraint_Error with "Unknown bot name given: " & Name;
         end Index_Of;
         procedure Get_Velocity
           (Num_Bots    :        Ictypes.Int;
            Active_Bot  :        Ictypes.Int;
            K           :        Ictypes.Double;
            Gv          :        Ictypes.Double;
            R           :        Ictypes.Double;
            Kv          :        Ictypes.Double;
            F           :        Ictypes.Double;
            Pose_X      : in out Ictypes.Double_Arrays.C_Array;
            Pose_Y      : in out Ictypes.Double_Arrays.C_Array;
            Pose_A      : in out Ictypes.Double_Arrays.C_Array;
            Velo_X      : in out Ictypes.Double_Arrays.C_Array;
            Velo_A      : in out Ictypes.Double_Arrays.C_Array;
            Goal_X      : in out Ictypes.Double_Arrays.C_Array;
            Goal_Y      : in out Ictypes.Double_Arrays.C_Array;
            Goal_Active : in out Ictypes.Int_Arrays.C_Array;
            Out_Velo_X  :    out Ictypes.Double;
            Out_Velo_A  :    out Ictypes.Double);
         pragma Import (C, Get_Velocity, "springed_team__get_velocity");

         Num_Bots : constant Positive :=
                      Positive (Bots.Length); -- For the base
         Pose_X   : Ictypes.Double_Arrays.C_Array (0 .. Num_Bots - 1);
         Pose_Y   : Ictypes.Double_Arrays.C_Array (0 .. Num_Bots - 1);
         Pose_A   : Ictypes.Double_Arrays.C_Array (0 .. Num_Bots - 1);
         Velo_X   : Ictypes.Double_Arrays.C_Array (0 .. Num_Bots - 1);
         Velo_A   : Ictypes.Double_Arrays.C_Array (0 .. Num_Bots - 1);
         Goal_X   : Ictypes.Double_Arrays.C_Array (0 .. Num_Bots - 1);
         Goal_Y   : Ictypes.Double_Arrays.C_Array (0 .. Num_Bots - 1);
         Goal_Active : Ictypes.Int_Arrays.C_Array (0 .. Num_Bots - 1) :=
                         (others => 0);
         Out_Velo_X,
         Out_Velo_A   : Ictypes.Double;
         Velo     : Types.Pose := Types.Null_Pose;
         Idx      : Natural := 0;
      begin
         for I in Bots.First_Index .. Bots.Last_Index loop
            declare
               Bot : constant Located_Agent.Object'Class :=
                       Located_Agent.Object'Class (Bots.Element (I));
            begin
               Pose_X (Idx) := Ictypes.Double (Bot.Get_Pose.X);
               Pose_Y (Idx) := Ictypes.Double (Bot.Get_Pose.Y);
               Pose_A (Idx) := Ictypes.Double (Bot.Get_Pose.A);
               Velo_X (Idx) := Ictypes.Double (Bot.Get_Velo.X);
               Velo_A (Idx) := Ictypes.Double (Bot.Get_Velo.A);
               if Bot.Has_Tasks then
                  Goal_X (Idx) := Ictypes.Double
                    (Sancta.Tasks.Positioned.Object
                       (Bot.Get_First_Task).Pose.X);
                  Goal_Y (Idx) := Ictypes.Double
                    (Sancta.Tasks.Positioned.Object
                       (Bot.Get_First_Task).Pose.Y);
                  Goal_Active (Idx) := 1;
               else
                  Goal_X (Idx) := 0.0;
                  Goal_Y (Idx) := 0.0;
               end if;
            end;
            Idx := Idx + 1;
         end loop;

         Get_Velocity (Ictypes.Int (Num_Bots),
                       Index_Of (This.Option (Option_Agent_Name)),
                       Ictypes.Double (This.K),
                       Ictypes.Double (This.Gv),
                       Ictypes.Double (This.R),
                       Ictypes.Double (This.Kv),
                       Ictypes.Double (This.F),
                       Pose_X, Pose_Y, Pose_A,
                       Velo_X, Velo_A,
                       Goal_X, Goal_Y, Goal_Active,
                       Out_Velo_X, Out_Velo_A);
         Velo.X := Types.Real (Out_Velo_X);
         Velo.A := Types.To_Angle (Types.Real (Out_Velo_A));
         This.Output (Provides_Velo_Goal, Ctypes.Pose'(Pose => Velo));
         Log ("Obtained velo for " & This.Option (Option_Agent_Name) & ": " &
              Debug2.To_String (Velo),
              Debug, Log_Section);
      end;
   end Perform;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      This.Period.Next (Next);
      if This.Exists (Requires_Team) then
         This.Perform (Ctypes.Teams (This.Input (Requires_Team)).Team);
      end if;
   end Run;

end Sancta.Ctree.Component.Springed_Team_vRAS;
