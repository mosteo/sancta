with Sancta.Component.Factory;

package body Sancta.Component.Gtk_Canvas is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Comp_Config)
      return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Subscribe (Requires_Drawable);

      This.Canvas := Agpl.Gdk.Managed.Drawing_Area.show
        (This.Sources, This.Option (Opt_Title, Def_Title));

      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   overriding procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
      use Agpl.Drawing;
   begin
      if Value in Drawable'Class then
         This.Sources.Draw (External_Tag (Value'Tag), Drawable'Class (Value));
      elsif Value in Data_Parcel'Class then
         declare
            Parcel : Data_Parcel'Class renames Data_Parcel'Class (Value);
         begin
            This.Sources.Draw
              (Image (Parcel.Owner) & ":" & (+Parcel.Label),
               Drawable'Class (Parcel.Datum.Ref.all));
         end;
      end if;

      This.Canvas.Draw (This.Sources);
   end Key_Stored;

end Sancta.Component.Gtk_Canvas;
