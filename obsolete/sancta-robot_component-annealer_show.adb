with Sancta.Config;
with Sancta.Distributed.Datastore;
with Sancta.Distributed.Types;
with Sancta.Draw;
with Sancta.Plugin.Annealer;
with Sancta.Plugin.Factory; pragma Elaborate_All (Sancta.Plugin.Factory);
with Sancta.Plugin.Shared_Database;

with Agpl.Trace; use Agpl.Trace;

package body Sancta.Plugin.Annealer_Show is

   use type Sancta.Costs;
   use type Distributed.Object_Key;

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Plugin.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      if Sancta.Config.Get_Plugin (Shared_Database.Plugin_Name) = null then
         raise Program_Error with
         "Plugin " & Plugin_Name &
         " requires plugin " & Shared_Database.Plugin_Name;
      else
         declare
            Database : constant Distributed.Datastore.Object_Access :=
                         Shared_Database.Object_Access
                           (Sancta.Config.Get_Plugin
                              (Shared_Database.Plugin_Name)).Get;
         begin
            Database.Listen (Annealer.Shared_Context_Key, This.Listener'Access);
         end;
      end if;

      return Plugin.Object_Access (This);
   end Create;

   -------------------
   -- On_Key_Stored --
   -------------------

   procedure On_Key_Stored
     (L     : in out Db_Listener;
      From  : in     Network.Node_Id;
      Key   : in     Distributed.Object_Key;
      Value : in     Distributed.Object_Data'Class;
      Meta  : in     Distributed.Object_Metadata)
   is
      pragma Unreferenced (From, Meta);
      This    : Object renames L.Parent.all;
      Dann    : Distributed.Types.Danneal renames
        Distributed.Types.Danneal (Value);
   begin
      Log ("Annealing context updated", Never);
      pragma Assert (Key = Annealer.Shared_Context_Key);
      if Dann.Ass_Cost < This.Best_Cost then
         This.Best_Cost := Dann.Ass_Cost;
         Draw.Draw_Assignment (Dann.Assignment, Dann.Agent_Costs.Ref.all);
      end if;
   end On_Key_Stored;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Plugin.Annealer_Show;
