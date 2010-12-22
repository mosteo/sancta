with Agpl.Drawing,
     Agpl.Tasking.Period,
     Player.Localize,
     Sancta.Component.Player_Client,
     Sancta.Component.Player_Iface;

package Sancta.Component.Player_Localize is

   Max_Hypoths : constant := 16;
   --  Great shit that I should remove ASAP

   Log_Section : constant String := "sancta.component.player_localize";

   Name : aliased constant Component_Name := "player_localize";

   Requires_Mu   : constant Internal_Key := "mu";
   --  This Mu will be used instead of the one in the hyp, if provided.
   --  Only useful if we're only interested in the covariance, e.g.

   Provides_Hyp  : constant Internal_Key := "hyp";  -- Hypothesis (below)

   Opt_Period  : constant Option_Attr := "period";
   Def_Period : constant Duration     := 0.1;

   Opt_Filter_Zeros : constant Option_Attr := "filter_zeros";
   Def_Filter_Zeros : constant Boolean     := False;
   --  Some p2 ifaces (i.e. localize) give spureous 0,0 poses before the
   --  filter starts running. This option prevents their being output.
   --  However, this also prevents a robot from being at exactly (0,0,0).
   --  So, lacking a better solution, you can disable it here

   procedure Register;

   type Hypothesis is
     new Data
     and Agpl.Drawing.drawable
   with record
      Hypothesis : Player.Localize.Hypothesis;
   end record;

private

   use Agpl;

   type Object is new Player_Iface.Object with record
      Period  : Tasking.Period.Object := Tasking.Period.Create (Def_Period);
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
   procedure Draw (This :        hypothesis;
                   D    : in out Agpl.Drawing.Drawer'Class);

end Sancta.Component.Player_Localize;
