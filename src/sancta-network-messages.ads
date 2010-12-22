--  For general messages not depending in other parts of the application.

with Sancta.Types;

with Agpl.Generic_Handle;
with Sancta.Component.Types;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Handle;
with Agpl.Protected_Datastore;
with Agpl; use Agpl;

with Ada.Calendar;

package Sancta.Network.Messages is

--   pragma Elaborate_Body;

   --  See Packets.txt for full details on members and usage of the message types.

   type Shutdown_Type is new Message with null record;
   Shutdown : constant Shutdown_Type := (null record);

   --  Manual correction of pose
   type Set_Pose_Type is new Message with
      record
         Pose : Types.Pose;
      end record;
   function Set_Pose (Pose : in Types.Pose) return Set_Pose_Type;

   --  Replace all tasks with this list
   type Set_Tasks_Type is new Message with
      record
         Jobs : Sancta.Tasks.Containers.Lists.List;
      end record;
   function Set_Tasks (This : in Sancta.Tasks.Containers.Lists.List) return Set_Tasks_Type;
   function Set_Task (This : in Sancta.Tasks.Object'Class) return Set_Tasks_Type;

   Clear_Tasks : constant Set_Tasks_Type :=
                   (Jobs => Sancta.Tasks.Containers.Lists.Empty_List);

   --  Alive ping
   type Hello_Type is new Message with null record;
   Hello : constant Hello_Type := (null record);

   --  Propose task for someone to do something with it
   type Propose_Task_Type is new Message with
      record
         Job : Sancta.Tasks.Handle.Object;
      end record;
   function Propose_Task (This : in Sancta.Tasks.Object'Class) return Propose_Task_Type;

   --  Notification of ended task
   type Task_Done_Type is new Message with
      record
         Id : Sancta.Tasks.Task_Id;
      end record;
   function Task_Done (This : in Sancta.Tasks.Task_Id) return Task_Done_Type;

   --  Laser scan
   type Laser_Type (Last : Natural) is new Message with
      record
         Timestamp     : Ada.Calendar.Time := Ada.Calendar.Clock;
         --  Time of capture
         Robot_Pose    : Types.Pose;
         --  Robot pose when captured
         Laser_Pose    : Types.Pose;
         --  Laser on robot pose
         Range_Scan    : Types.Range_Scan (1 .. Last);
         --  Relative readings to robot pose (not odometry)
      end record;
   function Laser (Scan : Component.Types.Range_Scan) return Laser_Type;

   --  For the redirect component
   package Datastore_Key_Handle is new
     Agpl.Generic_Handle (Agpl.Protected_Datastore.Object_Key);
   package Datastore_Object_Handle is new
     Agpl.Generic_Handle (Agpl.Protected_Datastore.Object_Data'Class,
                          Agpl.Protected_Datastore."=");

   type Redirect_Type is new Message with record
      Key : Datastore_Key_Handle.Object;
      Val : Datastore_Object_Handle.Object;
      Ack : Boolean := False;
   end record;
   function Redirect (Key : Agpl.Protected_Datastore.Object_Key;
                      Val : Agpl.Protected_Datastore.Object_Data'Class;
                      ACK : Boolean := False)
                      return Redirect_Type;
   pragma Inline (Redirect);

   type Redirect_Ack is new Message with record
      Key : Datastore_Key_Handle.Object;
      Val : Datastore_Object_Handle.Object;
   end record;

end Sancta.Network.Messages;
