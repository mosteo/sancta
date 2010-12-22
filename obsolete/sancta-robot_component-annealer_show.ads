--  Sample empty plugin to save-as when creating new ones.

with Sancta.Distributed;
with Sancta.Network;
with Sancta.Plugin.Root;

with Sancta;

package Sancta.Plugin.Annealer_Show is

   pragma Elaborate_Body;

   Plugin_Name : constant String := "annealer_show";

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   not overriding
   function Create (Config : in Agpl.Xml.Node)
                    return      Plugin.Object_Access;

   --  Any of the following subprograms can be removed entirely if they're not
   --  going to be used, since there's null defaults in the Root class.

private

   with Agpl; use Agpl;

   type Db_Listener (Parent : access Object) is new Distributed.Key_Listener
     with null record;

   procedure On_Key_Stored (L     : in out Db_Listener;
                            From  : in     Network.Node_Id;
                            Key   : in     Distributed.Object_Key;
                            Value : in     Distributed.Object_Data'Class;
                            Meta  : in     Distributed.Object_Metadata);

   type Object is new Root.Object with record
      Best_Cost :         Sancta.Costs := Sancta.Infinite;
      Listener  : aliased Db_Listener (Object'Access);
   end record;

end Sancta.Plugin.Annealer_Show;
