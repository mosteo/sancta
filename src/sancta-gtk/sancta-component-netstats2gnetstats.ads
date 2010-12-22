with Agpl.Gdk.Custom_Widget;
with Agpl.Gdk.Widget_Bundle;
with Sancta.Component.Cast;
with Sancta.Component.Netstats;

package Sancta.Component.Netstats2Gnetstats is

   --  Make it a widget

   Name : constant Component_Name := "netstats2gnetstats";

   type Stats is
     new Netstats.Stats
     and Agpl.Gdk.Custom_Widget.Remote
   with null record;

   overriding
   function Create (This : Stats) return Agpl.Gdk.Widget_Bundle.Object;

   overriding
   procedure Update (This : in out Stats;
                     Guts : in out Agpl.Gdk.Widget_Bundle.Object);

   function Cast (From : Netstats.Stats) return Stats;

   package Caster is
     new Sancta.Component.Cast (Name, Netstats.Stats, Stats, Cast);

end Sancta.Component.Netstats2Gnetstats;
