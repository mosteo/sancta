with Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Map.Qtree,
     Sancta.Map.Qtree.Builder,
--       Sancta.Map.Qtree.Draw,
     Sancta.Map.Smart;
with Sancta.Types;

package body Sancta.Component.Qmap is

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
      use Sancta.Types;

      This : constant Object_Access := new Object (Name'Access, Config);

      M    : constant Sancta.Map.Qtree.Object_Access :=
               new Sancta.Map.qtree.Object
                 (Sancta.Map.Qtree.Vicinities'Value
                    (This.Option (Option_Vicinity, "vicinity_6")));
   begin
      M.Set_Cell_Size (Min => Real'Value (This.Option (Option_Cell_Min)),
                       Max => Real'Value (This.Option (Option_Cell_Max)));

      Map.Qtree.Builder.From_Png
        (M.all,
         This.Option (Option_File),
         Xmin => X_Real'Value (This.Option (Option_X_Left)),
         Xmax => X_Real'Value (This.Option (Option_X_Right)),
         Ymin => Y_Real'Value (This.Option (Option_Y_Bottom)),
         Ymax => Y_Real'Value (This.Option (Option_Y_Top)),
         Buggy_Stage => Boolean'Value
           (This.Option (Option_Buggy_Stage, Default_Buggy_Stage'Img)));

--        M.Set_Options ((Draw_Mode => Map.Qtree.Solid_With_Skel, others => <>));

--        Map.Qtree.Draw.Draw (M.all);
--        delay 60.0;

      This.Output
        (Provides_Map,
         Types.Map_Data'(Data with Map.Smart.Bind (Map.Object_Access (M))));

      return Component.Object_Access (This);
   end Create;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Qmap;
