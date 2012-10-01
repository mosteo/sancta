with Ada.Unchecked_Deallocation;
with Agpl.Drawing.Figures;
with Player,
     Sancta.Component.Factory;
with Sancta.Component.Ctypes;
with Sancta.Types;
with Sancta.Types.Player;

package body Sancta.Component.Player_Localize is

   ----------------------
   -- Create_Interface --
   ----------------------

   function Create_Interface (This : Object) return Player_Client.Iface_Access
   is
      pragma Unreferenced (This);
   begin
      return new Player.Localize.Object;
   end Create_Interface;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Agpl.Xml.Node)
      return Component.Object_Access
   is
      This : constant Object_Access :=
               new Object (Name   => Name'Access,
                           Config => Config);
   begin
      This.Period.Set_Period
        (Duration'Value (This.Option (Opt_Period, Def_Period'Img)));

      This.Filter :=
        Boolean'Value (This.Option (Opt_Filter_Zeros, Def_Filter_Zeros'Img));

      This.Client.all.Sync;
      This.Run;

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
   begin
      This.Run;
      This.Period.Next (Next);
   end Run;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object)
   is
      type Hyp_Array_Access is access all Player.Localize.Hypothesis_Array;

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Player.Localize.Hypothesis_Array, Hyp_Array_Access);

      Hyps : Hyp_Array_Access;

      procedure Safe (Iface : Player_Client.Iface_Access);
      procedure Safe (Iface : Player_Client.Iface_Access) is
         Loc : Player.Localize.Object renames
           Player.Localize.Object (Iface.all);
      begin
         Hyps := new Player.Localize.Hypothesis_Array'(Loc.Get_Hypotheses);
      end Safe;

      use type Player.Double_Array;

      Mu : Sancta.Types.Pose;
   begin
      This.Execute (Safe'Access);

      for I in Hyps'Range loop
         if not This.Provided_And_Exists (Requires_Mu) then
            if This.Filter and then Hyps (I).Mean = (0.0, 0.0, 0.0) then
               Log ("Zero pose filtered out", Warning, Log_Section);
            else
--                 LOG ("SGRAIGGGGGGGGGGG MU", ALWAYS);
               This.Output (Provides_Hyp,
                            Hypothesis'(Data with Hyps (I)));
            end if;
         else
            Mu := Ctypes.Pose (This.Input (Requires_Mu)).Pose;
            declare
               use Sancta.Types.Player;
               Hyp : Player.Localize.Hypothesis := Hyps (I);
            begin
               Hyp.Mean (1) := +Mu.X;
               Hyp.Mean (2) := +Mu.Y;
               Hyp.Mean (3) := +Mu.A;

               if This.Filter and then Hyp.Mean = (0.0, 0.0, 0.0) then
                  Log ("Zero pose filtered out", Warning, Log_Section);
               else
--                    LOG ("SUPPPPPPLILLILILLIED MU", ALWAYS);
                  This.Output (Provides_Hyp,
                               Hypothesis'(Data with Hyp));
               end if;
            end;
         end if;
      end loop;

      Free (Hyps);
   end Run;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Hypothesis;
                   D    : in out Agpl.Drawing.Drawer'Class)
   is
      H : Player.Localize.Hypothesis renames This.Hypothesis;
      function "+" (X : Player.Double) return Float; pragma Inline ("+");
      function "+" (X : Player.Double) return Float is
      begin
         return Float (X);
      end "+";
   begin
      D.Set_Color ((0, 222, 0), 0);
      Agpl.Drawing.Figures.Create_Bivariate_Ellipse
        ((+H.Mean (1), +H.Mean (2)),
         +H.Mean (3),
         ((+H.Cov (1), 0.0), (0.0, +H.Cov (2)))).Draw (D);
   end Draw;

end Sancta.Component.Player_Localize;
