--  Sample empty component to save-as when creating new ones.

with Agpl.Tasking.Period,
     Sancta.Component.Player_Client,
     Sancta.Component.Player_Iface;

package Sancta.Component.Player_Position2d is

   Log_Section : constant String := "sancta.component.player_position2d";

   Name : aliased constant Component_Name := "player_position2d";

   Requires_Pose_Set  : constant Internal_Key := "pose_set";
   --  Use this to reset the internal odometry of the device (optional)
   Requires_Pose_Goal : constant Internal_Key := "pose_goal";
   Requires_Velo_Goal : constant Internal_Key := "velo_goal";
   --  Monitored; on update this is passed to the position2d device.

   Provides_Pose : constant Internal_Key := "pose";
   provides_Velo : constant Internal_Key := "velo";

   Option_Period  : constant Option_Attr := "period";
   Default_Period : constant Duration    := 0.1;

   Opt_Filter_Zeros : constant Option_Attr := "filter_zeros";
   Def_Filter_Zeros : constant Boolean     := True;
   --  Some p2 ifaces (i.e. localize) give spureous 0,0 poses before the
   --  filter starts running. This option prevents their being output.
   --  However, this also prevents a robot from being at exactly (0,0,0).
   --  So, lacking a better solution, you can disable it here

   procedure Register;

private

   use Agpl;

   type Object is new Player_Iface.Object with record
      Period  : Tasking.Period.Object := Tasking.Period.Create (Default_Period);
      Enabled : Boolean := False;
      Filter  : Boolean := Def_Filter_Zeros;
   end record;

   type Object_Access is access all Object'Class;

   overriding
   function Create_Interface (This : Object) return Player_Client.Iface_Access;

   function Create (Config : Agpl.Xml.Node)
                    return   Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   not overriding
   procedure Run (This : in out Object);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Player_Position2d;
