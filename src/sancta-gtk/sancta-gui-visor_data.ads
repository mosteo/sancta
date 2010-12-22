--  This encapsulates all data needed by visor widgets.

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Finalization; use Ada.Finalization;

with Agpl.Chronos;
with Agpl.Drawing.Multisource;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Sancta;
with Sancta.Agent_Proxy;
with Sancta.Auctioneer;
with Sancta.Gui.Config;
with Sancta.Mission_Tracker;
with Sancta.Network.Layer;
with Sancta.Network.Messages;
with Sancta.Tasks;
with Sancta.Types;

with Gtk;
with Gtk.Tree_Store; use Gtk.Tree_Store;

package Sancta.Gui.Visor_Data is

   package Laser_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Network.Messages.Laser_Type, Network.Messages."=");

   type Robot_Data is record
      Agent     : Agent_Proxy.Object;
      Cost      : Sancta.Costs := 0.0; -- Cached cost, said by the robot

      Status    : Ustring := +"Unknown";

      Gui_Color : Ustring; -- Use it to allocate its drawing color.

      Laser_Scans : Laser_Vectors.Vector;
      --  Keeps the latest laser readings for display

      Last_Seen : Agpl.Chronos.Object;
   end record;

   type Robot_Access is access all Robot_Data;
   type Robot_Array is array (Positive range <>) of Robot_Access;

   package Robot_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Robot_Access);

   type Object is tagged limited private;
   type Object_Access is access all Object;

   procedure Assign_Color (This : in out Robot_Data);
   --  Pre: Name must be assigned.

   function Config (This : Object) return Sancta.Gui.Config.Object;

   procedure Set_Config (This : in out Object;
                         Opts :        Sancta.Gui.Config.Object);

   function Get_Mission (This : access Object)
                         return        Mission_Tracker.Object_Access;

   function Get_Robot (This : access Object;
                       Name : in     String) return Robot_Access;
   --  Get a direct access to the named robot,
   --  if it didn't existed it will return null.

   function Get_Robot_Color (Name : in String) return String;
   --  Returns a suitable color name for GTK based on first character of Name

   function Get_Logs (This : in Object) return Gtk_Tree_Store;
   --  Store with the received logs

   function Get_Robots (This : in Object) return Robot_Array;
   --  Obtain pointers to all robots stored.

   function Locate_Robot (This : access Object;
                          Name : in     String) return Robot_Access;
   --  Get a direct access to the named robot,
   --  if it didn't existed it will be allocated.

   function Locate_Robot (This : Object_Access;
                          Pose : in     Types.Pose;
                          Dist : in     Float := 5.0) return Robot_Access;
   --  Locates the closest robot to Pose, within Dist distance to Pose.
   --  If no closer robot than Dist, null can be returned.

   Sender_Column  : constant := 0; First_Column : constant := 0;
   Date_Column    : constant := 1;
   Level_Column   : constant := 2;
   Section_Column : constant := 3;
   Text_Column    : constant := 4; Last_Column  : constant := Text_Column;

   --  Auctioning things
   procedure Add_Task_For_Auction (This : in out Object;
                                   Job  : in     Sancta.Tasks.Object'Class);
   procedure Run_Auctions (This : in out Object);

   --  Network things  --
   function Get_Link (This : Object_Access) return Network.Layer.Object_Access;
   procedure Set_Link (This : in out Object;
                       Link : in     Network.Layer.Object_Access);

   --  Drawing
   function Drawer (This : Object_Access)
                    return Agpl.Drawing.Multisource.Object_Access;

   function Is_Recording (This : Object) return Boolean;

   procedure Set_Recording (This : in out Object;
                            Rec  :        Boolean := True);

private

   type Object is new Limited_Controlled with record
      Robots : Robot_Maps.Map; -- All data directly concerning to robots.

      Logs   : Gtk_Tree_Store;
      --  The columns to store are:
      --  Sender_Id
      --  Date of arrival
      --  Warning level
      --  Text

      Drawer : aliased Agpl.Drawing.Multisource.Object;

      Link   : Network.Layer.Object_Access;

      Mission   : aliased Mission_Tracker.Object;

      Auctioneer : Sancta.Auctioneer.Object_Access;

      Options   : Sancta.Gui.Config.Object;

      Recording : Boolean := False;
   end record;

   procedure Initialize (This : in out Object);
   procedure Finalize (This : in out Object);

end Sancta.Gui.Visor_Data;
