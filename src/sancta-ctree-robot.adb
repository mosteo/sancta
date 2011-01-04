with Agpl.Trace,
     Sancta.Map.Bitmap,
     Sancta.Tasks.Positioned,
     Sancta.Tasks.Starting_Pose,
     Sancta.Types,
     Sancta.Types.Operations;

use Sancta.Types,
    Sancta.Types.Operations;

package body Sancta.Ctree.Robot is

   ----------------
   -- Get_Prefix --
   ----------------

   function Get_Prefix return Map.Path is
--        M : Map.Bitmap.Object'Class renames
--          Map.Bitmap.Object'Class (The_Map.Ref.all);
      Prefix : Map.Path;
   begin
--        Prefix.Append (M.Loc (0, 9));
--        Prefix.Append (M.Loc (1, 9));
--        Prefix.Append (M.Loc (2, 9));

      return Prefix;
   end Get_Prefix;

   ------------
   -- Create --
   ------------

   function Create (Name : String;
                    Pose : Types.Pose) return Object'Class
   is
      This : Object;
   begin
      This.Set_Name (Name);
      This.Set_Pose (Pose);
      return This;
   end Create;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This     : Object;
      Ini, Fin : Sancta.Tasks.Object'Class)
      return Sancta.Costs
   is
      function Cost (Ini, Fin : Types.Pose) return Sancta.Costs is
      begin
         if The_Map.Is_Null then
            raise Program_Error with "No map!";
            return Sancta.Costs (Distance (Ini, Fin));
         else
            declare
               use Sancta.Map;
               Loc_Ini : constant Location'Class :=
                           The_Map.Ref.Nearest_Location (Ini);
               Loc_Fin : constant Location'Class :=
                           The_Map.Ref.Nearest_Location (Fin);
            begin
               return The_Map.Ref.Get_Cost_Between (Loc_Ini, Loc_Fin);
            end;
         end if;
      end Cost;
   begin
      if Ini in Sancta.Tasks.Starting_Pose.Object'Class then
         if Fin in Sancta.Tasks.Starting_Pose.Object'Class then
            return 0.0;
         else
            return Cost (This.Get_Pose, Tasks.Positioned.Object (Fin).Pose);
         end if;
      else
         return Cost (Tasks.Positioned.Object (Ini).Pose,
                      Tasks.Positioned.Object (Fin).Pose);
      end if;
   exception
      when others =>
         Log ("Ini tag: " & External_Tag (Ini'Tag), Error);
         Log ("Fin tag: " & External_Tag (Fin'Tag), Error);
         raise;
   end Get_Cost;

   ----------------
   -- Set_Mobile --
   ----------------

   procedure Set_Mobile (This   : in out Object;
                         Mobile :        Boolean := True)
   is
   begin
      This.Mobile := Mobile;
   end Set_Mobile;

   ---------------
   -- Is_Mobile --
   ---------------

   function Is_Mobile (This : Object) return Boolean is
   begin
      return This.Mobile;
   end Is_Mobile;

end Sancta.Ctree.Robot;
