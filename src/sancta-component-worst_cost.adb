with Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Map,
     Sancta.Map.Bitmap;

package body Sancta.Component.Worst_Cost is

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   -------------
   -- Process --
   -------------

   procedure Process
     (This : in out Object)
   is
      use Component.Types,
          Map.Bitmap;
      M : Map.Bitmap.Object renames
        Map.Bitmap.Object (Map_Data (This.Input (Requires_Map)).Map.Ref.all);
      Worst : Costs := 0.0;
   begin
      for R in M.Get_Data'Range loop
         for C in M.Get_Data'Range (2) loop
            if M.Get_Data (R, C) = Free then
               declare
                  P : constant Map.Path_With_Cost :=
                        M.Best_Path
                          (M.Nearest_Location
                             (Pose (This.Input (Requires_From_Pose)).Pose),
                           M.Loc (C, R));
               begin
                  if P.Cost > Worst then
                     Worst := P.Cost;
                     Log ("Current worst:" & Worst'Img, Always, Log_Section);
                  end if;
               end;
            end if;
         end loop;
      end loop;
      Log ("Done" & Worst'Img, Always, Log_Section);
   end Process;

   ------------
   -- Inputs --
   ------------

   function Inputs
     (This : Object)
      return Internal_Key_Array
   is
      pragma Unreferenced (This);
   begin
      return (Requires_Map'Access,
              Requires_From_Pose'Access);
   end Inputs;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
   begin
      return
        Component.Object_Access
          (Object_Access'(new Object (Name'Access, Config)));
   end Create;

end Sancta.Component.Worst_Cost;
