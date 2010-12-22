with Player,
     Player.Position2d,
     Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Debug2,
     Sancta.Types;
with Sancta.Types.Player;

package body Sancta.Component.Player_Position2d is

   ----------------------
   -- Create_Interface --
   ----------------------

   function Create_Interface (This : Object) return Player_Client.Iface_Access
   is
      pragma Unreferenced (This);
   begin
      return new Player.Position2d.Object;
   end Create_Interface;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Agpl.Xml.Node)
      return Component.Object_Access
   is
      This : constant Object_Access :=
               new Object (Name   => Name'Access,
                           Config => Config);
   begin
      This.Subscribe (Requires_Pose_Goal);
      This.Subscribe (Requires_Velo_Goal);
      This.Subscribe (Requires_Pose_Set);

      if This.Exists (Option_Period) then
         This.Period.Set_Period (Duration'Value (This.Option (Option_Period)));
      end if;

      This.Filter :=
        Boolean'Value (This.Option (Opt_Filter_Zeros, Def_Filter_Zeros'Img));

      This.Client.all.Sync;
      This.Run;

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
   begin
      This.Run;
      This.Period.Next (Next);
   end Run;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object)
   is
      use Sancta.Types;
      use Sancta.Types.Player;

      Pose : Sancta.Types.Pose;
      Velo : Sancta.Types.Pose;

      procedure Safe (Iface : Player_Client.Iface_Access) is
         P2d : Player.Position2d.Object renames
           Player.Position2d.Object (Iface.all);
      begin
         Pose := +P2D.Get_Pose;
         Velo := +P2D.Get_Velocity;
      end Safe;

   begin
      --  This.Client.all.Sync;
      --  Not sure if this is really needed...

      This.Execute (Safe'Access);

      if This.Filter and then Pose = Sancta.Types.Origin then
         Log ("Zero pose filtered out", Warning, Log_Section);
      else
         This.Output (Provides_Pose,
                      Component.Types.Robot_Pose'(Data with (Pose)));
         This.Output (Provides_Velo,
                      Component.Types.Pose'(Data with (Velo)));

         Log ("Pose idx" & This.Index'Img & ": " & Debug2.To_String (Pose),
              Debug, Log_Section);
         Log ("Velo idx" & This.Index'Img & ": " & Debug2.To_String (Velo),
              Debug, Log_Section);
      end if;
   end Run;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      use Sancta.Types;
      use Sancta.Types.Player;

      P    : constant player.Pose :=
        +Component.Types.Pose (Value).Pose;

      procedure Safe (Iface : Player_Client.Iface_Access) is
         P2d  : Player.Position2d.Object renames
           Player.Position2d.Object (Iface.all);
      begin
         if not This.Enabled then
            P2d.Enable;
            This.Enabled := True;
         end if;
         if Key = Requires_Pose_Goal then
            P2d.Set_Cmd_Pose (P);
            Log ("Move to " &
                 Debug2.To_String (Component.Types.Pose (Value).Pose),
                 Debug, Log_Section);
         elsif Key = Requires_Velo_Goal then
            Log ("Velo set to " &
                 Debug2.To_String (Component.Types.Pose (Value).Pose),
                 Debug, Log_Section);
            P2d.Set_Cmd_Vel (P);
         elsif Key = Requires_Pose_Set then
            P2d.Set_Pose (P);
            Log ("Pose reset to " &
                 Debug2.To_String (Component.Types.Pose (Value).Pose),
                 Always, Log_Section);
         else
            raise Program_Error with "Unexpected key";
         end if;
      end Safe;

   begin
      This.Execute (Safe'Access);
   end Key_Stored;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Player_Position2d;
