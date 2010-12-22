with Ada.Containers.Indefinite_Vectors;
with Agpl.Chronos,
     Agpl.Drawing,
     Agpl.Drawing.Multisource,
     Agpl.Tasking.Period,
     Agpl.Types,
     Player.Graphics2d,
     Player.Types,
     Sancta.Component.Player_Client,
     Sancta.Component.Player_Iface,
     Sancta.Containers,
     Sancta.Map,
     Sancta.Map.Smart;

use Sancta.Containers;

package Sancta.Component.Player_Graphics2d is

   Log_Section : constant String := "sancta.component.player_graphics2d";

   --  IMPORTANT:
   --  this uses double buffering to avoid flicker. In order to do so, two
   --  graphics2d models are needed.
   --  See Player.Graphics2d.Double for details!

   Name : aliased constant Component_Name := "player_graphics2d";

   Requires_Queue : constant Internal_Key := "queue";
   --  Data dumped here is drawn, see type Actions below
   --  Use either this or the multisource, but not both
   --  Queue is internally used by the multisource, so it's mandatory.

   Requires_Queue_Draw : constant Internal_Key := "queue_draw";
   --  Drawables or parcels to be dumped here.
   --  Drawables will use their tag for uniqueness, further differentiation
   --  will require a parcel (see redirect_listener or copy)
   pragma Highly_Inneficient ("Lots of copying going on here");

   Provides_Drawer : constant Internal_Key := "drawer";
   --  Alternative direct use

   Opt_Period : constant Option_Attr := "period";
   Def_Period : constant Duration    := 0.1;
   --  If /= 0, a flush of the multisource drawer is forced once this time
   --  passes if a key has been updated.

   procedure Register;

   type Actions is abstract new Data with null record;
   not overriding
   procedure Draw (This :        Actions;
                   G2d  : in out Player.Graphics2d.Object'Class) is abstract;
   --  Base type for actions that this object Queue understands...

   --  Some basic drawings are provided here:

   type Action_Clear is new Actions with null record;

   type Action_Flush is new Actions with null record;
   Do_Action_Flush : constant Action_Flush;
   --  Will send all pending drawings to screen.
   --  Necessary to minimize flicker.
   --  Don't forget this or nothing will get drawn!

   type Action_Polyline is new Actions with private;

   function Polyline (Points : Player.Types.Point_2d_Vector;
                      Color  : Player.Types.Player_Color_Type := (0, 0, 0, 0))
                      return   Action_Polyline;

   function Rectangle (Corner1,
                       Corner2 : Player.Point_2d;
                       Color   : Player.Types.Player_Color_Type := (0, 0, 0, 0))
                       return    Action_Polyline;

   type Action_Polygon is new Actions with private; -- filled polyline
   function Polygon (Points : Player.Types.Point_2d_Vector;
                     Color  : Player.Types.Player_Color_Type := (0, 0, 0, 0))
                     return   Action_Polygon;

   type Action_Goals is new Actions with record
      Goals : Tc.Lists.List;
   end record;

   type Action_Assignment is new Actions with record
      Agents : Ac.Lists.List;
   end record;

   type Action_Path is new Actions with record
      Map   : Sancta.Map.Smart.Object;
      Path  : Sancta.Map.Path;
      Color : Player.Types.Player_Color_Type := (0, 100, 200, 200);
   end record;

   package Action_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Actions'Class);

   subtype Action_Vector is Action_Vectors.Vector;

   type Player_Drawable is limited interface;

   procedure Player_Draw (This    :     Player_Drawable;
                          Actions : out Action_Vector) is null;

private

   type Object is new Player_Iface.Object and Agpl.Drawing.Drawer with record
      Drawer     : aliased Agpl.Drawing.Multisource.Object;
      Last_Flush : Agpl.Chronos.Object;
      Period     : Agpl.Tasking.Period.Object;

      Color      : Agpl.Types.Rgb_Triplet;
      Alpha      : Agpl.Types.Unsigned_8;
   end record;
   type Object_Access is access all Object'Class;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   overriding
   procedure Draw_Line (This   : in out Object;
                        X1, Y1,
                        X2, Y2 : Float);

   overriding
   procedure Fill_Rectangle
     (This   : in out Object;
      X1, Y1,
      X2, Y2 : Float);

   overriding
   procedure Set_Color (This  : in out Object;
                        Rgb   :        Agpl.Types.Rgb_Triplet;
                        Alpha :        Agpl.Types.Unsigned_8);

   overriding
   procedure Write (This : in out Object;
                    X, Y : Float;
                    Utf8 : String);

   type Action_Polyline is new Actions with record
      Color  : Player.Types.Player_Color_Type := (0, 0, 0, 0); -- Black
      Points : Player.Types.Point_2d_Vector;
   end record;

   type Action_Polygon is new Actions with record
      Color  : Player.Types.Player_Color_Type := (0, 0, 0, 0); -- Black
      Points : Player.Types.Point_2d_Vector;
   end record;

   overriding
   function Create_Interface (This : Object) return Player_Client.Iface_Access;

   overriding
   procedure Draw (This :        Action_Clear;
                   G2d  : in out Player.Graphics2d.Object'Class);

   overriding
   procedure Draw (This :        Action_Flush;
                   G2d  : in out Player.Graphics2d.Object'Class);

   overriding
   procedure Draw (This :        Action_Polyline;
                   G2d  : in out Player.Graphics2d.Object'Class);

   overriding
   procedure Draw (This :        Action_Polygon;
                   G2d  : in out Player.Graphics2d.Object'Class);

   overriding
   procedure Draw (This :        Action_Goals;
                   G2d  : in out Player.Graphics2d.Object'Class);

   overriding
   procedure Draw (This :        Action_Assignment;
                   G2d  : in out Player.Graphics2d.Object'Class);

   overriding
   procedure Draw (This :        Action_Path;
                   G2d  : in out Player.Graphics2d.Object'Class);

   function Create (Config : Agpl.Xml.Node)
                    return   Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   Do_Action_Flush : constant Action_Flush := (others => <>);

end Sancta.Component.Player_Graphics2d;
