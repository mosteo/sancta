--  Gets a drawable and pumps it into a drawer.

with Sancta.Component.Root;

private with Agpl.Drawing.Multisource;
private with Sancta.Network.Inbox;
private with Sancta.Network.Layer;

package Sancta.Component.Draw is

   Log_Section : constant String := "sancta.component.draw";

   Name : aliased constant Component_Name := "draw";

   Requires_drawer   : constant Internal_Key := "drawer";
   --  Thing that will do the final drawing. It's a multidrawer
   --  SCTypes.Drawer

   Requires_drawable : constant Internal_Key := "drawable";
   --  Stored drawable at some key
   Requires_Link     : constant Internal_Key := "link";
   --  Link on which receive drawables (Redirect messages)

   --  Either the drawable or the link has to be supplied

   Option_Channel    : constant Option_Attr  := "channel";
   --  only needed if it is listening into link

   Option_Label      : constant Option_Attr  := "label";
   --  The id used for the drawable. Different things to be drawn require
   --  different labels. Only necessary for Drawable, discarded for Link
   --  since sent Drawables already carry a label
   --  NOTE: drawing order is by lexicographical sort of ID

   procedure Register;

private

   type Object is new Root.Object with record
      Drawer : Agpl.Drawing.Multisource.Object_Access;
      Link   : Network.Layer.Object_Access;
      Inbox  : aliased Network.Inbox.Object;
   end record;

   function Create (Config : Comp_Config)
                    return   Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Draw;
