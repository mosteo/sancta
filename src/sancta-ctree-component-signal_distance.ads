with Sancta.Component.Root; use Sancta.Component;

private with Ada.Calendar;
private with Ada.Containers.Ordered_Maps;
private with Agpl.Tasking.Period;
private with Sancta.Ctree;
private with Sancta.Component.Environment;
private with Sancta.Netlistener;
private with Sancta.Network.Layer;
private with Sancta.Types;

package Sancta.Ctree.Component.Signal_Distance is

   Log_Section : constant String := "sancta.ctree.component.signal_distance";

   Name : aliased constant Component_Name := "ctree_signal_distance";

   Requires_Base_Pose : constant Internal_Key := "base_pose";
   Requires_Link      : constant Internal_Key := "link";

   Provides_Signal    : constant Internal_Key := "signal";
   --  nctypes.signal

   Provides_Full_Signal : constant Internal_Key := "full_signal";
   --  nctypes.full_signal
   --  Both RTWMP and a simulated signal can be locally provide a full picture
   --  of the network; so there's no need to burden ourselves with propagating
   --  that stuff.

   Opt_Period : constant Option_Attr := "period";
   Def_Period : constant Duration    := 0.2;

   Opt_Drop_Dist  : constant Option_Attr := "drop_dist";
   --  Distance at which signal Q is 0%

   type Drop_Models is (Linear);
   Opt_Drop_Model : constant Option_Attr := "drop_model";
   Def_Drop_Model : constant String      := "linear";
   --  How to compute signal, given distance. Unused for now (it's the only one)

   --  NOTE: the noises defined next are GLOBALLY INCONSISTENT. They're seen
   --  differently by each node.

   Opt_Random_Noise_Amplitude : constant Option_Attr := "rnd_noise_amp";
   Def_Random_Noise_Amplitude : constant Float       := 5.0;
   --  plus/minus Percent to randomly add to any generated signal

   Opt_Bias_Amplitude : constant Option_Attr := "bias_amp";
   Def_Bias_Amplitude : constant Float       := 2.0;
   --  A time-varying bias
   Opt_Bias_Period    : constant Option_Attr := "bias_period";
   Def_Bias_Period    : constant Duration    := 2.0;
   --  Minimum time needed for bias to go from -Amp to +Amp

   Opt_Channel     : constant Option_Attr := "channel";
   --  Channel for CTree comms.

   Opt_Base_Id     : constant Option_Attr := "base_id";
   --  The id for the fake base agent

   procedure Register;

private

   use Sancta;

   package Id_Pose_Maps is new Ada.Containers.Ordered_Maps
     (Node_Id, Types.Pose, "<", Sancta.Types."=");

   package Bias_Maps is new Ada.Containers.Ordered_Maps
     (Unordered_Node_Pair, Float);

   type Object is tagged;

   --  There's a mix of the old way, which used links from ourselves to all others,
   --  and the new way that simply stores all heard poses, and uses them to
   --  compute all distances in Output_Full_Links

   type Listener_Type (Link   : not null access Network.Layer.Object'Class;
                       Parent : access Object) is
     new Netlistener.Object (Link) with null record;

   overriding
   procedure Process_Incoming_Packet (This : in out Listener_Type;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Link   : access Network.Layer.Object'Class)
     is new Root.Object (Name, Config) with record
      Self       : access Object := Object'Unchecked_Access;
      Listener   : Listener_Type (Link, Object'Access);
      Id         : Node_Id; -- ourselves
      Drop_Dist  : Types.Real;

      Pose_Valid : Boolean := False;
      Pose       : Types.Pose; -- our pose
      Poses      : Id_Pose_Maps.Map; -- all poses
      Links      : Ctree.Id_Q_Maps.Map;
      Noise_Amp  : Float := Def_Random_Noise_Amplitude;
      Biases     : Bias_Maps.Map;
      Bias_Amp   : Float := Def_Bias_Amplitude;
      Bias_Delta : Float :=
                     (Def_Bias_Amplitude * 2.0 / Float (Def_Bias_Period) *
                        Float (Def_Period));
      --  Max units per period that bias can change

      Period     : Agpl.Tasking.Period.Object :=
                     Agpl.Tasking.Period.Create (Def_Period);
   end record;

   function Create (Config : Comp_Config;
                    Env    : Environment.Object)
                    return   Sancta.Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   not overriding
   procedure Add_Link (This     : in out Object;
                       Other_Id :        Node_Id;
                       Other_P  :        Types.Pose);

   not overriding
   procedure Output_Full_Links (This : in out Object);

end Sancta.Ctree.Component.Signal_Distance;
