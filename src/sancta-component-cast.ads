with Sancta.Component.Factory;
with Sancta.Component.Root;

generic
   Register_Name : String;
   type Required (<>) is new Data with private;
   type Provided (<>) is new Data with private;
   with function Cast (From : Required) return Provided;
package Sancta.Component.Cast is

   --  Just a cast from Required to Provided.
   --  Parcels are transparently managed.
   --  Type check is done so non-matching inputs are let through uncasted.
   --    This allows daisy-chaining several casts in a common parcel flow.

   Log_Section    : constant String := "sancta.component.pose2robot_pose";

   Name : aliased constant Component_Name := Register_Name;

   Requires_In  : constant Internal_Key := "in";
   Provides_Out : constant Internal_Key := "out";

   Opt_Passthrough : constant Option_Attr := "passthrough";
   Def_Passthrough : constant Boolean     := False;
   --  If False, non-matching inputs are reported, parceled or not.

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

   type Object is new Root.Object with record
      Pass : Boolean := Def_Passthrough;
   end record;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   Create_Access : constant Factory.Object_Creator := Create'Access;

end Sancta.Component.Cast;
