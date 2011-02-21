with Sancta.Component;
with Sancta.Component.Root;

private with Ada.Calendar;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Ordered_Maps;
private with Agpl.Chronos;
private with Agpl.Containers.Ordered_Multidimensional_Maps;
private with Agpl.Containers.String_Vectors;
private with Agpl.Drawing.Multisource;
private with Agpl.Gdk.Managed.Drawing_Area;
private with Agpl.Gdk.Widget_Bundle;
private with Agpl.Monitor;
private with Agpl.Tasking.Period;
private with Agpl.Ustrings;
private with Glade.Xml;
private with Gtk.Box;
private with Gtk.Label;
private with Sancta.Gtk.Link_History;
private with Sancta.Gui.Visor;
private with Sancta.Network;
private with Sancta.Network.Inbox;
private with Sancta.Network.Layer;
private with Sancta.Network.Qualities;

package Sancta.Ctree.Component.Console is

   Log_Section : constant String := "Sancta.Ctree.Component.ctree_console";
   Det_Section : constant String := "Sancta.Ctree.Component.ctree_console.details";

   use Sancta.Component;

   Name : aliased constant Component_Name := "ctree_console";

   Requires_Full_Signal   : constant Internal_Key := "full_signal"; -- nctypes.full_signal
   Requires_Link          : constant Internal_Key := "link";
   Requires_Local_Parcels : constant Internal_Key := "local_parcels";
   --  Drawables/Widgets for the local node views. Wrapped in parcel.
   Requires_Global_Parcels: constant Internal_Key := "global_parcels";
   --  Drawables/Widgets for the map global view. Wrapped in parcel.
   Requires_Tabs          : constant Internal_Key := "tabs";
   --  Drawables/widgets for the notebook

   Provides_Map_Drawer  : constant Internal_Key := "map_drawer"; -- sctypes.drawer
                                                                 --  Drawer for the map area

   Emits_Laser_Forwarding : constant External_Key := "laser_forwarding";
   --  This key is relayed to act as switch for a remote redirect component.

   Opt_Channel : constant Option_Attr := "channel";
   --  Channel on which to listen for redirects/drawables, send commands/switchs

   --  There's a hardcoded "gui" channel, inherited from the legacy map view,
   --  which is hardcoded...

   Opt_Quality_Threshold : constant Option_Attr := "quality_threshold";

   Opt_Period : constant Option_Attr := "period";
   Def_Period : constant Duration    := 0.2;

   Opt_Glade_Xml : constant Option_Attr := "glade_xml";
   Def_Glade_Xml : constant String :=
     "/home/jano/prog/sancta/src/sancta-gtk/ctree_console_v2.glade";
   --  Supply here path to definition file

   procedure Register;

private

   use Agpl;
   use Agpl.Ustrings;
   use Standard.Gtk.Box;
   use Sancta;

   subtype Two_D is Positive range 1 .. 2;

   type Robot_Canvas is limited record
      Buffer : Agpl.Drawing.Multisource.Object;
   end record;
   type Canvas_Access is access all Robot_Canvas;

   package Id_Canvas_Maps is new
     Ada.Containers.Ordered_Maps (Node_Id, Canvas_Access);

   package Id_VBox_Maps is new
     Ada.Containers.Ordered_Maps (Node_Id, Gtk_VBox);

   package Id_Area_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Node_Id, Agpl.Gdk.Managed.Drawing_Area.Handle,
        "<", Agpl.Gdk.Managed.Drawing_Area."=");

   package String_Area_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (String, Agpl.Gdk.Managed.Drawing_Area.Handle,
        "<", Agpl.Gdk.Managed.Drawing_Area."=");

   package Label_Matrices is new Agpl.Containers.Ordered_Multidimensional_Maps
     (Two_D, Natural, Standard.Gtk.Label.Gtk_Label,
      "<", Standard.Gtk.Label."=");

   package Linkhist_Matrices is new Agpl.Containers.Ordered_Multidimensional_Maps
     (Two_D, Natural, Sancta.Gtk.Link_History.Object_Access, "<", Sancta.Gtk.Link_History."=");

   type Node_Widget_Key is record
      Id   : Node_Id;
      Name : Ustring;
   end record;

   function "<" (L, R : Node_Widget_Key) return Boolean;

   package Node_Widget_Maps is
     new Ada.Containers.Ordered_Maps
       (Node_Widget_Key, Agpl.Gdk.Widget_Bundle.Object,
        "<",             Agpl.Gdk.Widget_Bundle."=");

   type Object is new Root.Object with record
      Period : Tasking.Period.Object := Tasking.Period.Create (Def_Period);

      Signal_Timer : Agpl.Chronos.Object;

      Link   : Sancta.Network.Layer.Object_Access;
      Chan   : Sancta.Network.Channel;
      Inbox  : aliased Sancta.Network.Inbox.Object;

      Nodes  : Agpl.Containers.String_Vectors.Vector;
      --  We get them from the configured network link

      Q_Threshold : Sancta.Network.Qualities.Quality;

      Gui                   : Glade.XML.Glade_XML;
      Instant_Signal_Labels : Label_Matrices.Map;
      --  Text matrix with signal qualities

      Link_Histories        : Linkhist_Matrices.Map;

      Local_DAreas          : Id_Area_Maps.Map;
      --  Drawing area for each robot
      Local_Canvases        : Id_Canvas_Maps.Map;
      --  Canvases for local robot surroundings (pose, laser...)
      Local_Vboxes          : Id_Vbox_Maps.Map;
      --  The vbox for each robot into which append custom widgets
      Local_Widgets         : Node_Widget_Maps.Map;
      --  The remote widgets for each node
      Tabs                  : String_Area_Maps.Map;
      --  The tabs in the notebook, as handles.

      Map_Drawer            : Agpl.Drawing.Multisource.Object_Access;

      Visor                 : Sancta.Gui.Visor.Object_Access;
      General_View          : Sancta.Gui.Visor.Widget_Container;

      Mutex                 : aliased Agpl.Monitor.Fake_Semaphore;
      --  Shitty shit, but simpler at this stage...
   end record;

   function Create (Config : Comp_Config)
                    return   Sancta.Component.Object_Access;

   not overriding
   procedure Send (This : Object;
                   Msg  : Sancta.Network.Message'Class);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Ctree.Component.Console;
