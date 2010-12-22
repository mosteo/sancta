with Sancta.Datastore;
with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Types;

package body Sancta.Component.Logger_Pose is

   function To_String (This : in Agpl.Protected_Datastore.Object_Data'Class)
                       return    String
   is
      P : Types.Pose renames Datastore.Pose (This).Pose;
   begin
      return To_String (P.X) & " " & To_String (P.Y) & " " & To_String (P.A);
   end To_String;

end Sancta.Component.Logger_Pose;
