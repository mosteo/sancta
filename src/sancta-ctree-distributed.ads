with Agpl.Drawing.Multisource;
with Agpl.Reflection.Booleans;
with Agpl.Trace.File;
with Sancta.Ctree.Connectivity_Matrix;
with Sancta.Ctree.Signal_Maps;
with Sancta.Ctree.Stats;
with Sancta.Ctree.Tree_Navigator;
with Sancta.Assignment;
with Sancta.Component;
with Sancta.Containers; use Sancta.Containers;
with Sancta.Map;
with Sancta.Map.Smart;
with Sancta.Netlistener;
with Sancta.Network;
with Sancta.Tasks.Handle;
with Sancta.Types;

private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Vectors;
private with Agpl.Chronos;
private with Agpl.Generic_Handle;
private with Agpl.Ustrings;

package Sancta.Ctree.Distributed is

   --  The ultimate CTREE implementation

   Log_Section : constant String := "sancta.ctree.distributed";
   Det_Section : constant String := "sancta.ctree.distributed.detail";

   package R renames Agpl.Reflection;

   --  Globals for selective drawing
   --  while waiting for a better solution.
   Ctree_Status_Draw_Signal : R.Booleans.Object := R.Booleans.Value
     ("ctree_status_draw_signal", True);
   Ctree_Status_Draw_Locations : R.Booleans.Object := R.Booleans.Value
     ("ctree_status_draw_locations", True);
   Ctree_Status_Draw_Targets : R.Booleans.Object := R.Booleans.Value
     ("ctree_status_draw_targets", True);
   Ctree_Status_Draw_Tree : R.Booleans.Object := R.Booleans.Value
     ("ctree_status_draw_tree", True);
   Ctree_Status_Draw_Links : R.Booleans.Object := R.Booleans.value
     ("ctree_status_draw_links", True);

   type Mobile_Link_Policies is (Ordered, Grouped);
   pragma Unimplemented;
   --  When Ordered, each robot monitors only its sucessors.
   --  When Grouped, as long as any of the mobiles has link to the relays, all move.
   --     This is designed with fading in mind. The expected outcome is that, at
   --     some point, only the last mobile will have connection and will stop.
   --     Maladies that can happen if this assumption fails? One of the non-last
   --     mobiles may be the last one to lose connection...

   type Config_Type is record
      Loc_Dist_Threshold : Sancta.Types.Real := 1.0;
      --  Distance to consider a location reached
      Target_Dist_Threshold : Sancta.Types.Real := 1.0;
      --  Distance to consider a target reached

      Here_Dist_Threshold   : Sancta.Types.Real := 1.0;
      Near_Dist_Threshold   : Sancta.Types.Real := 3.0;
      --  Distances for a robot to be consider HERE and NEARBY
      --  We don't move towards a HERE robot.
      --  We don't move further apart from a NEARBY robot.

      Signal_Threshold : Signal_Q := 50.0;
      --  Stopping signal quality in the 0..100 range.

      Fixed_Relay_Period    : Duration := 5.0;
      --  Once this time elapses in relay state, we can't leave it even if
      --  signal improves...

      Rot_Vel : Sancta.Types.Angle := 0.3;

      Update_Period         : Duration := 0.5;
      --  Period between sending of update messages

      Parking_Range         : Sancta.Types.Real := 4.0;
      --  Distance around parking point considered parking.

      Use_All_Relays        : Boolean := True;
      pragma Unimplemented;
      --  When false, only the last relay is monitored.
      --  When true, if any relay provides signal, robots move on.

      Mobile_Link_Policy    : Mobile_Link_Policies := Ordered;
   end record;

   type Roles is (Base, Tail, Relay, Head, Tailhead);
   --  Tailhead is for the special case of a single stand-alone robot.
   --  Basically for testing.

   type States is (Pending_Relay, Pending_Waiting, Pending_Free,
                   Done_Relay,    Done_Waiting,    Done_Free);
   --  Relays are stationed because of signal
   --  Waiting are stationed because of distance margings
   --  Free are free to move
   --  This is in regard to current task, not about going up/down in tree

   Moving_States : constant array (States) of Boolean :=
                     (Pending_Free | Done_Free => True, others => False);

   Pending_States : constant array (States) of Boolean :=
                      (Pending_Relay .. Pending_Free => True, others => False);

   Relay_States : constant array (States) of Boolean :=
                    (Pending_Relay | Done_Relay => True, others => False);

   Done_States    : constant array (States) of Boolean :=
                      (Done_Relay .. Done_Free => True, others => False);

   type Object is new Sancta.Netlistener.Object with private;

   procedure Create (This      : in out Object;
                     Config    : Config_Type;
                     Base      : Sancta.Node_Id;
                     Base_Pose : Sancta.Types.Pose;
                     Map       : Sancta.Map.Smart.Object;
                     Channel   : Sancta.Network.Channel);

   not overriding
   function Get_Config (This : Object) return Config_Type;

   not overriding
   procedure Set_Config (This : in out Object; Config : Config_Type);

   type Mission_States is (Mission_Waiting,   -- Role determination
                           Mission_Snafu,     -- i.e. running...
                           Mission_Completed, --
                           Mission_Aborted);

   not overriding
   procedure Step (This  : in out Object);
   --  Execute one algorithm iteration

   not overriding
   procedure Set_Pose (This : in out Object;
                       Pose :        Sancta.Types.Pose);

   not overriding
   procedure Set_Qualities (This : in out Object;
                            Q    :        Id_Q_Maps.Map);
   --  Provide quality, with a particular smoothing function applied

   not overriding
   procedure Set_Ordered_Tasks (This : in out Object;
                                Jobs :        Tc.Lists.List);

   not overriding
   procedure Set_Navigator
     (This : in out Object;
      Nav  :        Sancta.Ctree.Tree_Navigator.Handle.Object);

   not overriding
   procedure Set_Drawer
     (This : in out Object;
      Draw :        Agpl.Drawing.Multisource.object_access);

   not overriding
   function Status (This : Object) return Mission_States;

   not overriding
   procedure Switch_Status (This : in out Object; New_Status : States);

   type Robot_Actions is (Wait, Move, Turn);
   type Robot_Orders (Action : Robot_Actions := Wait) is record
      case Action is
         when Wait => null;
         when Move =>
            Goal : Sancta.Types.Pose;
         when Turn =>
            Velo : Sancta.Types.Pose;
      end case;
   end record;

   not overriding
   function Orders (This : Object) return Robot_Orders;

   not overriding
   function To_Assignment (This : Object) return Sancta.Assignment.Object;

   not overriding
   function Pending_Tasks (This : Object) return Tc.Lists.List;

   --  Messaging

   type Ctree_Msg is interface;

   procedure Process (Msg  : Ctree_Msg;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class) is abstract;

   type Msg_Robot_Local_Update is new Sancta.Network.Message and Ctree_Msg with
      record
         Seq     : Natural; -- To check 1-hop lost packets
         From    : Sancta.Node_Id;
         Pose    : Sancta.Types.Pose;
         Status  : States;
         Parked  : Boolean;
         --  Needed to avoid unnecesary turns
      end record;
   --  This message is periodically sent by a robot to its neighbors.

   overriding
   procedure Process (Msg  : Msg_Robot_Local_Update;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class);

   type Ways is (To_Head, To_Tail);

   type Msg_Robot_Global_Update is
     new Sancta.Network.Message
     and Ctree_Msg
   with
      record
         Way  : Ways;
         Id   : Sancta.Node_Id;
         Pose : Sancta.Types.Pose;
      end record;
   --  This message propagates all the way to head and tail.
   --  Nodes need the locations of others to build signal maps.

   overriding
   procedure Process (Msg  : Msg_Robot_Global_Update;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class);

   type Operator_Actions is (Go, Stop, Park, Cancel);

   type Msg_Operator_Control (Action : Operator_Actions) is
     new Sancta.Network.Message and Ctree_Msg with
      record
         case Action is
            when Go | Stop | Cancel => null;
            when Park =>
               Towards : Sancta.Types.Angle;
         end case;
      end record;

   overriding
   procedure Process (Msg  : Msg_Operator_Control;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class);

   type Status_Draw is new Sancta.Component.Data and Agpl.Drawing.Drawable
   with private;

   not overriding
   function Get_Draw_Status (This : Object) return Status_Draw'Class;

   type Node_Status is
     new Sancta.Component.Data
     and Agpl.Drawing.Drawable with private;

   function Pose   (X : Node_Status) return Sancta.Types.Pose;
   function Q      (X : Node_Status) return Sancta.Network.Qualities.Quality;
   function Report (X : Node_Status) return String;
   function Threshold (X : Node_Status) return Sancta.Network.Qualities.Quality;

   not overriding
   function Get_Node_Status (This : Object) return Node_Status'Class;

   not overriding
   function Is_Commanding_Robot (This : Object) return Boolean;
   --  Says if the robot must be commanded.
   --  Always false for the Base node.

   not overriding
   function Quality_Map (This : Object) return Signal_Maps.Quality_View;

private

   use Sancta;

   type Msg_Setup_Request is new Sancta.Network.Message and Ctree_Msg with
      record
         Pose : Types.Pose; -- Distance to base will determine ordering and roles.
      end record;

   overriding
   procedure Process (Msg  : Msg_Setup_Request;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class);

   type Msg_Setup_Reply is new Sancta.Network.Message and Ctree_Msg with
      record
         Tail_To_Head : Id_Vectors.Vector;
      end record;

   overriding
   procedure Process (Msg  : Msg_Setup_Reply;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class);

   type Msg_Mission_Update is new Sancta.Network.Message and Ctree_Msg with
      record
         Prev_Target   : Sancta.Tasks.Handle.Object;
         Curr_Target   : Sancta.Tasks.Handle.Object;
         Backloc       : Sancta.Map.Location_Handle.Object;
         Branch_Target : Sancta.Tasks.Handle.Object;
         Mission_Done  : Boolean;
      end record;
   --  Periodical, backwards, by the head robot.
   --  All nodes must have the needed info to replicate branches and so on.
   --  When the Curr_Target changes, robots must update accordingly...
   --  Remember: backloc changes on targets change.
   --  Branch_Target is used to signify the current branch target

   overriding
   procedure Process (Msg  : Msg_Mission_Update;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class);

   type Msg_Heartbeat is new Sancta.Network.Message and Ctree_Msg with
      record
         Seq : Natural; -- to check N-hop lost packets
      end record;
   --  Sent by the head robot towards base

   overriding
   procedure Process (Msg  : Msg_Heartbeat;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class);

   type Robot_Info is record
      From       : Sancta.Node_Id;
      Succ       : Sancta.Node_Id;
      Role       : Roles;
      Status     : States;
      Parked     : Boolean;
      Pose       : Sancta.Types.Pose;
      Pred_Dist  : Types.Real;
      Succ_Pose  : Sancta.Types.Pose;
      Q          : Signal_Q; -- To successor
      Loc        : Sancta.Map.Location_Handle.Object;
      Relay_Time : Duration;
   end record;
   pragma Pack (Robot_Info); -- Pitiful attempt at saving some bytes?

   package Id_Info_Maps is new
     Ada.Containers.Ordered_Maps (Node_Id, Robot_Info);

   package Info_Vectors is new
     Ada.Containers.Vectors (Positive, Robot_Info);

   type Msg_Head_To_Tail_Global_Status is new Sancta.Network.Message and Ctree_Msg with
      record
         Info       : Id_Info_Maps.Map;
         Target_Loc : Sancta.Map.Location_Handle.Object;
      end record;
   --  This message is sent from head to tail. Each robot getting it adds its
   --  own information

   overriding
   procedure Process (Msg  : Msg_Head_To_Tail_Global_Status;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class);

   type Msg_Tail_To_Head_Status is new Sancta.Network.Message and Ctree_Msg with
      record
         Exhausted : Boolean;
         Log_Time  : Duration;
      end record;
   --  Sent from tail to head to determine team strained

   overriding
   procedure Process (Msg  : Msg_Tail_To_Head_Status;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class);

   package Op_Msg_Handle is new Agpl.Generic_Handle (Msg_Operator_Control);

   type Neighbor_Context (Parent : access Object) is tagged record
      Id     : Sancta.Node_Id := No_Node;
      Dist   : Types.Real     := 9999.0;
      Q      : Signal_Q       := Signal_Q'First;
      Pose   : Types.Pose     := Types.Origin;
      Status : States         := Pending_Free;
      Parked : Boolean        := True; -- Means at starting point
   end record;

   type Robot_Updates is limited record
      Pose : Types.Pose;
   end record;
   --  Yet another robot status info record. This will hold the global data
   --  all robots know about each other, propagated in the Msg_Robot_Global_Update

   type Robot_Updates_Access is access Robot_Updates;

   package Id_Updates_Maps is new Ada.Containers.Ordered_Maps
     (Node_Id, Robot_Updates_Access);

   type Object_Access is access all Object;

   type Object is new Sancta.Netlistener.Object with record
      Self           : Object_Access := Object'Unchecked_Access;
      --  For the Rosen trick

      Config         : Config_Type;
      Setup_Info     : Info_Vectors.Vector; -- using only during startup phase.
      Setup_Last     : Id_Vectors.Vector;   -- last ordering received
      --  This, during execution, contains all the robot IDs from tail to head.
      --  NOTE: IT DOES NOT CONTAIN THE BASE ID.

      Updates        : Id_Updates_Maps.Map;
      --  Global info that every robot knows about the others, base included.
      --  Look at the accessor functions in order not to manipulate this directly
      --  Upon initialization complete, this should hold valid values and be usable.

      Role           : Roles;
      Map            : Sancta.Map.Smart.Object;
      Loc            : Sancta.Map.Location_Handle.Object;
      --  The one we are moving towards
      Pose           : Types.Pose;
      Status         : States := Pending_Free;
      Jobs           : Tc.Lists.List; -- Pending only!!
      Jobs_Done      : Tc.Lists.List; -- Done

      Base_Id        : Node_Id;
      Base_Pose      : Types.Pose;
      Navigator      : Ctree.Tree_Navigator.Handle.Object;
      Branch         : Sancta.Map.Path;
      Branch_Target  : Tasks.Handle.Object;
      Prev_Target    : Tasks.Handle.Object;
      Target         : Tasks.Handle.Object;
      Backloc        : Sancta.Map.Location_Handle.Object;

      Update_Timer   : Agpl.Chronos.Object; -- Network updates
      Relay_Timer    : Agpl.Chronos.Object;
      --  Timer for becoming a totally stopped relay.
      Exhaust_Timer  : Agpl.Chronos.Object;
      --  Timer for detection of exhausted robots
      Silence_Timer  : Agpl.Chronos.Object;
      --  Time since last network message arrival
      --  Should be upper bounded if network never fails
      Setup_Timer    : Agpl.Chronos.Object; -- Timer for setup msgs

      Mission_Status : Mission_States := Mission_Waiting;
      Mission_Timer  : Agpl.Chronos.Object;
      --  Absolute time since mission start

      Pred           : Neighbor_Context (Object'Access);
      --  Predecesor robot (to head)
      Succ           : Neighbor_Context (Object'Access);
      --  Sucessor robot (to tail)

      Qs             : Id_Q_Maps.Map;
      --  Qualities as seen from this robot.

      Q_Map          : Signal_Maps.Map_Family;
      --  Mapping of signal environ.

      Operator_Msg   : Op_Msg_Handle.Object :=
                         Op_Msg_Handle.Set
                           (Msg_Operator_Control'(Action => Stop));

      Channel             : Network.Channel;
      Msg_Seq             : Natural := 0; -- Sequence of head updates

      One_hop_Msg_Count   : Natural := 0;
      One_Hop_Lost_Msgs   : Natural := 0;
      One_Hop_Last_In_Seq : Natural := 0;
      --  To count losses of Pred neighbor packets

      N_Hop_Msg_Count     : Natural := 0;
      N_Hop_Lost_Msgs     : Natural := 0;
      N_Hop_Last_In_Seq   : Natural := 0;
      --  To count losses of head packets

      Last_Global_Status  : Msg_Head_To_Tail_Global_Status;
      Last_Mission_Update : Msg_Mission_Update;
      Last_Reverse_Status : Msg_Tail_To_Head_Status;

      First_Log_Data      : Boolean := True; -- Only to print #header for logs
      First_Log_Signal    : Boolean := True; -- As above
      Log_Time            : Duration := 0.0;
      Log_Delta           : Agpl.Chronos.Object;
      --  So all nodes log synced with base time

      Stats               : Sancta.Ctree.Stats.Object;
      Dummy_Links         : Connectivity_Matrix.Object;
      --  Not really used; necessary to call Stats members.

      Draw           : Agpl.Drawing.Multisource.Object_Access;
      Draw_Flip           : Boolean := False;

      Commanding_Robot    : Boolean := False;
      --  Just to compatibilize running with other softwares
      --  True when last msg was Go/Park, false otherwise

      Logger_Ctree        : Agpl.Trace.File.Object;
      Logger_Signal       : Agpl.Trace.File.Object;
   end record;

   not overriding
   function Bot (This : Object; Id : Node_Id)
                 return access constant Robot_Updates;

   not overriding
   function Bot_Ptr (This : access Object; Id : Node_Id)
                     return access Robot_Updates;

   not overriding
   procedure Bot_Init (This : in out Object;
                       Id   :        Node_Id);
   --  Ensure that Updates info for this bot is present

   not overriding
   function Succ_Loc (This : Object) return Sancta.Map.Location'Class;

   not overriding
   function Is_Head (This : Object) return Boolean;
   --  True when Role = Head or Role = Tailhead

   not overriding
   function Is_Tail (This : Object) return Boolean;
   --  True when Role = Tail or Role = Tailhead

   overriding
   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);

   not overriding
   procedure Report (This : Object);

   not overriding
   function One_Liner_Report (This : Object) return String;

   not overriding
   procedure Log_Data (This : in out Object);

   not overriding
   procedure Log_Signal (This : in out Object);

   not overriding
   procedure Send_Update (This : in out Object);

   not overriding
   procedure Send_Setup (This : in out Object);

   not overriding
   function M (This : Object) return Sancta.Map.Object_Access;
   pragma Inline (M);

   function Image (Orders : Robot_Orders) return String;

   not overriding
   procedure Do_Draw (This : in out Object);

   not overriding
   procedure Check_Links (This : in out Object);

   procedure Update_Neighbor (This : in out Neighbor_Context;
                              Msg  :        Msg_Robot_Local_Update'Class);

   type Relative_Dist is (Here, Near, Far);

   not overriding
   function Rel_Dist (This : Object; Dist : Types.Real) return Relative_Dist;

   not overriding
   function Target_Pose (This : Object) return Types.Pose;

   not overriding
   function Parked (This : Object) return Boolean;

   not overriding
   procedure Location_Back (This : in out Object);
   --  Move the objetive location one step back

   not overriding
   procedure Location_Forward (This : in out Object);
   --  Move the objetive location one step forward

   not overriding
   function Loc_Pose (This : Object) return Types.Pose;

   not overriding
   function Team_Exhausted (This : Object) return Boolean;

   not overriding
   procedure End_Mission (This : in out Object);

   not overriding
   function Action_Park (This : Object) return Robot_Orders;

   not overriding
   function Action_Turn (This    : Object;
                         Towards : Types.Pose)
                         return     Robot_Orders;

   not overriding
   function Action_Turn (This    : Object;
                         Towards : Types.Angle)
                         return     Robot_Orders;

   type Status_Draw is new Sancta.Component.Data and Agpl.Drawing.Drawable
   with record
      Base_Pose  : Types.Pose;
      Navigator  : Tree_Navigator.Handle.Object;
      Jobs_Pend  : Tc.Lists.List;
      Jobs_Done  : Tc.Lists.List;
      Global     : Msg_Head_To_Tail_Global_Status;
   end record;

   overriding
   procedure Draw (This : Status_Draw; Into : in out Agpl.Drawing.Drawer'Class);

   type Node_Status is
     new Sancta.Component.data
     and Agpl.Drawing.drawable
   with record
      Role      : Roles;
      Pose      : Types.Pose;
      Status    : States;
      Succ_Q    : Signal_Q; -- To predecessor
      Threshold : Signal_Q;
      Report    : Agpl.Ustrings.Ustring;
   end record;
   pragma Pack (Node_Status); -- Minimize BW? Bah...

   overriding
   procedure Draw (This : Node_Status; Into : in out Agpl.Drawing.Drawer'Class);

end Sancta.Ctree.Distributed;
