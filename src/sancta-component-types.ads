with Agpl.Drawing;
with Agpl.Drawing.Multisource;

with Sancta.Agent;
with Sancta.Assignment;
with Sancta.Containers;
with Sancta.Cost_Cache;
with Sancta.Datastore;
with Sancta.Map;
with Sancta.Map.Smart;
with Sancta.Network.Layer;
with Sancta.Tasks.Handle;
with Sancta.Types;

package Sancta.Component.Types is

   --  All input/output types should be defined here.
   --  Unless there's some good reason to keep them isolated somewhere else.

   subtype Bool is Datastore.Bool;
   function Boolean (Value : Standard.Boolean) return Bool; pragma Inline (Boolean);

   subtype Dstring is Datastore.Dstring;

   function New_Dstring (S : String) return Dstring
                         renames Sancta.Datastore.New_Dstring;

   subtype Pose is Datastore.Pose;

   type Robot_Pose is new Datastore.Pose and Agpl.Drawing.Drawable with null record;
   procedure Draw (This :        Robot_Pose;
                   D    : in out Agpl.Drawing.Drawer'Class);

--  subtype Poses is Datastore.Pose;

   --  This package is used by the generic smoother component.
   --  Not really tested yet, you're supposed to instantiate it with the real
   --  type with the precision you need.
   generic
      type Item is digits <>;
   package Reals is
      type Real is abstract new Data with null record;
      function Denormalize (X : Item) return Real is abstract;
      function Normalize   (X : Real) return Item is abstract;
   end Reals;

   type Agent is new Data with record
      Agent : Sancta.Agent.Object_Access;
   end record;

   type Map_Data is new Data and Agpl.Drawing.Drawable with record
      Map : Sancta.Map.Smart.Object;
   end record;
   procedure Draw (This :        Map_Data;
                   D    : in out Agpl.Drawing.Drawer'Class);

   type Job is new Data with record
      Job : Tasks.Handle.Object;
   end record;

   type Task_List is new Data with record
      Tasks : Containers.Tc.Lists.List;
   end record;

   type Teams is new Data with record
      Team : Assignment.Object;
   end record;

   type Paths is new Data with record
      Path : Map.Path;
   end record;

   type Range_Scan is new Data with record
      Scan_Id              : Integer;
      --  To uniquely identify a reading
      Sensor_Pose_On_Robot : Sancta.Types.Local_Pose;
      --  Pose of the laser on robot
      Scan                 : Sancta.Types.Smart_Full_Scan_Access.Object;
      --  Avoid copy by using a handle
      Robot_Pose           : Sancta.Types.Pose;
      --  A pose that may come with the scan, hardware dependent
      Robot_External_Pose  : Sancta.Types.Pose;
      --  Another pose, supplied by odometry/gps/whatever
      --  Local or global? According to needs I guess
   end record;

   type Posed_Scan (Length : Natural) is new Data and Agpl.Drawing.Drawable with
      record
         Robot_Pose          : Sancta.Types.Pose;
         Laser_Pose_On_Robot : Sancta.Types.Pose;
         Samples             : Sancta.Types.Range_Scan (1 .. Length);
         Ini, Fin            : Sancta.Types.Angle;
      end record;

   procedure Draw (This :        Posed_Scan;
                   D    : in out Agpl.Drawing.Drawer'Class);

   type Cost_Cache (Ptr : Sancta.Cost_Cache.Object_Access) is
     new Data with null record;

   type Drawer (Drawer : Agpl.Drawing.Multisource.Object_Access) is
     new Data with null record;

   type Network (Link : Sancta.Network.Layer.Object_Access)
     is new Data with null record;

end Sancta.Component.Types;
