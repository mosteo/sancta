with Player,
     Player.Graphics2d.Double,
     Sancta.Assignment,
     Sancta.Component.Factory,
     Sancta.Player.Draw;
with Sancta.Types.Player;
with Sancta.Component.Types;

package body Sancta.Component.Player_Graphics2d is

   package P renames Standard.Player;

   ----------------------
   -- Create_Interface --
   ----------------------

   function Create_Interface (This : Object) return Player_Client.Iface_Access
   is
      pragma Unreferenced (This);
   begin
      Log ("Creating new P.Graphics2d.Double.Object", Always, Log_Section);
      return new P.Graphics2d.Double.Object;
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
      Log ("Creating Sancta.Comp.Player_Graphics2D", Always, Log_Section);
      if This.Provided (Provides_Drawer) then
         This.Output (Provides_Drawer,
                      Types.Drawer'(Drawer => This.Drawer'Access));
      end if;

      This.Subscribe (Requires_Queue);
      This.Subscribe (Requires_Queue_Draw);

      This.Period.Set_Period
        (Duration'Value (This.Option (Opt_Period, Def_Period'Img)));

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
   begin
      This.Period.Next (Next);

      if This.Drawer.Is_Dirty then
--           This.Output (Requires_Queue,
--                        Action_Clear'(others => <>));
         This.Drawer.Flush (This);
         This.Output (Requires_Queue,
                      Do_Action_Flush);
      end if;
   end Run;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
   begin
      if Key = Requires_Queue then
         declare
            Action : Actions'Class renames Actions'Class (Value);

            procedure Safe (Iface : Player_Client.Iface_Access) is
            begin
               Action.Draw (P.Graphics2d.Object (Iface.all));
            end Safe;
         begin
            This.Execute (Safe'Access);

            if Action in Action_Flush then
               This.Last_Flush.Reset;
            elsif This.Last_Flush.Elapsed > 10.0 then
               Log ("No recent graphic flush!", Debug, Log_Section);
            end if;
         end;
      elsif Key = Requires_Queue_Draw then
         if Value in Agpl.Drawing.Drawable'Class then
            This.Drawer.Draw (External_Tag (Value'Tag),
                              Agpl.Drawing.Drawable'Class (Value));
         elsif Value in Data_Parcel'Class then
            declare
               Parcel : Data_Parcel'Class renames Data_Parcel'Class (Value);
            begin
--                 Log ("Drawing a " & External_Tag (Parcel.Datum.Ref.all'Tag) &
--                      " from " & Image (Parcel.Owner) & " labeled " & (+Parcel.Label),
--                      Always);
               --  We presume it's a drawable!
               This.Drawer.Draw (Image (Parcel.Owner) & ":" & (+Parcel.Label),
                                 Agpl.Drawing.Drawable'Class (Parcel.Datum.Ref.all));
            end;
         else
            Log ("Discarding undrawable: " & External_Tag (Value'Tag), Warning);
         end if;
      else
         raise Program_Error with "Unknown subscribed key?";
      end if;
   end Key_Stored;

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

   procedure Draw (This :        Action_Clear;
                   G2d  : in out P.Graphics2d.Object'Class)
   is
      pragma Unreferenced (This);
   begin
      G2d.Clear;
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Action_Polyline;
                   G2d  : in out P.Graphics2d.Object'Class)
   is
      P : constant Standard.Player.Types.Point_2d_Vector := This.Points;
   begin
      G2d.Set_Color     (This.Color);

      --  Duplicate last segment to force missing pixel:
      --  It's not working anyway
--        pragma Expensive ("Perhaps its overkill");
--        P.Append (P.Element (P.Last_Index - 1));
      G2d.Draw_Polyline (P);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Action_Polygon;
                   G2d  : in out P.Graphics2d.Object'Class)
   is
   begin
      G2d.Set_Color     (This.Color);
      G2d.Fill_Polygon (This.Points);
   end Draw;

   --------------
   -- Polyline --
   --------------

   function Polyline (Points : P.Types.Point_2d_Vector;
                      Color  : P.Types.Player_Color_Type := (0, 0, 0, 0))
                      return   Action_Polyline
   is
   begin
      return (Actions with Color, Points);
   end Polyline;

   -------------
   -- Polygon --
   -------------

   function Polygon (Points : P.Types.Point_2d_Vector;
                     Color  : P.Types.Player_Color_Type := (0, 0, 0, 0))
                     return   Action_Polygon is
   begin
      return (Actions with Color, Points);
   end Polygon;

   ---------------
   -- Rectangle --
   ---------------

   function Rectangle (Corner1,
                       Corner2 : P.Point_2d;
                       Color   : P.Types.Player_Color_Type := (0, 0, 0, 0))
                       return    Action_Polyline
   is
      V  : P.Types.Point_2d_Vector;
      P1 : constant P.Point_2d := (Corner2.X, Corner1.Y);
      P2 : constant P.Point_2d := (Corner1.X, Corner2.Y);
   begin
      V.Append (Corner1);
      V.Append (P1);
      V.Append (Corner2);
      V.Append (P2);
      V.Append (Corner1);
      return Polyline (V, Color);
   end Rectangle;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Action_Goals;
                   G2d  : in out P.Graphics2d.Object'Class)
   is
   begin
      Player.Draw.Draw_Goals (G2d, This.Goals);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Action_Path;
                   G2d  : in out P.Graphics2d.Object'Class)
   is
   begin
      Player.Draw.Draw_Path (G2d,
                             This.Map.Ref.all,
                             This.Path,
                             This.Color);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Action_Assignment;
                   G2d  : in out P.Graphics2d.Object'Class)
   is
   begin
      Player.Draw.Draw_Plan (G2d, Assignment.Create (This.Agents));
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Action_Flush;
                   G2d  : in out P.Graphics2d.Object'Class)
   is
      pragma Unreferenced (This);
   begin
      P.Graphics2d.Double.Object'Class (G2d).Flush;
   end Draw;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (This   : in out Object;
                        X1, Y1,
                        X2, Y2 : Float)
   is
      Line : Action_Polyline;
      use Standard.Player;
      use Standard.Player.Types;
      use Sancta.Types.Player;
   begin
      Line.Color :=
        (Red   => Uint8 (This.Color.R),
         Green => Uint8 (This.Color.G),
         Blue  => Uint8 (This.Color.B),
         Alpha => Uint8 (This.Alpha));
      Line.Points.Append ((+X1, +Y1));
      Line.Points.Append ((+X2, +Y2));
      This.Output (Requires_Queue, Line);
   end Draw_Line;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle
     (This   : in out Object;
      X1, Y1,
      X2, Y2 : Float)
   is
      P : Action_Polygon;
      use Standard.Player;
      use Standard.Player.Types;
      use Sancta.Types.Player;
   begin
      P.Color :=
        (Red   => Uint8 (This.Color.R),
         Green => Uint8 (This.Color.G),
         Blue  => Uint8 (This.Color.B),
         Alpha => Uint8 (This.Alpha));
      P.Points.Append ((+X1, +Y1));
      P.Points.Append ((+X1, +Y2));
      P.Points.Append ((+X2, +Y2));
      P.Points.Append ((+X2, +Y1));
      This.Output (Requires_Queue, P);
   end Fill_Rectangle;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color (This  : in out Object;
                        Rgb   :        Agpl.Types.Rgb_Triplet;
                        Alpha :        Agpl.Types.Unsigned_8)
   is
   begin
      This.Color := Rgb;
      This.Alpha := Alpha;
   end Set_Color;

   procedure Write (This : in out Object;
                    X, Y : Float;
                    Utf8 : String)
   is
      pragma Unreferenced (This, X, Y, Utf8);
   begin
      Log ("Text not implemented in Graphics2D", Debug, Log_Section);
   end Write;

end Sancta.Component.Player_Graphics2d;
