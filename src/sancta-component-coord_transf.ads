--  Change reference frames 2d

with Sancta.Component.Root;

package Sancta.Component.Coord_Transf is

   Log_Section : constant String := "sancta.component.coord_transf";

   Name : aliased constant Component_Name := "coord_transf";

   Requires_pose1 : constant Internal_Key := "pose1";
   requires_pose2 : constant Internal_Key := "pose2";

   Requires_Transf1 : constant Internal_Key := "transf1_to_2";
   Requires_Transf2 : constant Internal_Key := "transf2_to_1";

   Provides_Target1 : constant Internal_Key := "target1_to_2";
   Provides_Target2 : constant Internal_Key := "target2_to_1";

   procedure Register;

private

   type Object is new Root.Object with record
      Ready : Boolean := False;
   end record;

   function Create (Config : Comp_Config)
                    return   Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Coord_Transf;
