with Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Component.Utils,
     Sancta.Map.Bitmap,
     Sancta.Map.Cache,
     Sancta.Map.Smart;

with Agpl.Trace;
use Agpl.Trace;

package body Sancta.Component.Bitmap is

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
      use Utils;

      This : constant Object_Access := new Object (Name'Access, Config);
      M    : constant Sancta.Map.Bitmap.Object_Access :=
               new Sancta.Map.Bitmap.Object
                 (Sancta.Map.Bitmap.Vicinities'Value
                    (This.Option (Option_Vicinity, "vicinity_6")));
   begin
      M.From_Png (This.Option (Option_File, "file not supplied"),
                    Sancta.Map.Bitmap.Stage_Loader);
      M.Set_Cell_Size (Option (This.all, Option_Cell_Size, 0.0));

      if Option (This.all, Option_Compute_Costs, False) then
         begin
            Sancta.Map.Cache.Load (M.all, M.all);
            Log ("Loaded cached costs.", Informative);
         exception
            when E : others =>
               Log ("Costs not cached: " & Report (E), Informative);
               M.Finalize;
               M.From_Png (This.Option (Option_File, "file not supplied"),
                           Map.Bitmap.Stage_Loader);
               M.Set_Cell_Size (Option (This.all, Option_Cell_Size, 0.0));
               Log ("Computing costs...", Informative);
               M.Compute_Costs;
               Log ("Costs computed.", Informative);
               Map.Cache.To_File (M.all, M.all);
               Log ("Costs cached.", Informative);
         end;
      end if;

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

end Sancta.Component.Bitmap;
