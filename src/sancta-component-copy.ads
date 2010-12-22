with Sancta.Component.Root;

package Sancta.Component.Copy is

   --  Outputs one input with a new name.
   --  See also Merger, Generic_Mix, Y_or_else_X

   Name       : aliased constant Component_Name := "copy";

   Requires_X : constant Internal_Key := "x";
   Provides_Y : constant Internal_Key := "y";

   Opt_Wrap  : constant Option_Attr := "wrap";
   Def_Wrap  : constant Boolean     := False;
   Opt_Label : constant Option_Attr := "label";
   --  If wrap is True, the copied object is wrapped in a parcel with given label.
   --  Useful for drawables and so.

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   type Object is new Root.Object with record
      Wrap  : Boolean := Def_Wrap;
      Label : Ustring;
   end record;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Copy;
