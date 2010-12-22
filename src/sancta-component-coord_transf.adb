with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Debug2;
with Sancta.Types.Transformations;

package body Sancta.Component.Coord_Transf is

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
      return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Subscribe (Requires_Transf1);
      This.Subscribe (Requires_Transf2);
      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      use Sancta.Types.Transformations;
   begin
      if This.Ready or else
        (This.Exists (Requires_Pose1) and then This.Exists (Requires_Pose2))
      then
         This.Ready := True;
         declare
            Pose1 : constant Types.Pose :=
                      Ctypes.Pose (This.Input (Requires_Pose1)).Pose;
            Pose2 : constant Types.Pose :=
                      Ctypes.Pose (This.Input (Requires_Pose2)).Pose;
            Goal  : constant Types.Pose := Ctypes.Pose (Value).Pose;
            Goalt :          Types.Pose;
         begin
            if Key = Requires_Transf1 then
               Goalt := Change_Frame (Pose2, Pose1, Goal);
               This.Output (Provides_Target1, Ctypes.Pose'(Pose => Goalt));
            else
               Goalt := World_To_Odom (Pose1, Pose2, Goal);
               This.Output (Provides_Target2, Ctypes.Pose'(Pose => Goalt));
            end if;
            Log (Debug2.To_String (Goal) & " --> " & Debug2.To_String (Goalt),
                 Debug, Log_Section);
         end;
      else
         Log ("Dropping input because of missing data for transformation",
              Warning, Log_Section);
      end if;
   end Key_Stored;

end Sancta.Component.Coord_Transf;
