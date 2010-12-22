--  Sample empty plugin to save-as when creating new ones.

with Sancta.Plugin.Root;

private with Agpl.Types.Ustrinsg;

package Sancta.Plugin.Skeleton is

   Plugin_Name : constant String := "skeleton";

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   not overriding
   function Create (Config : in Agpl.Xml.Node)
                    return      Plugin.Object_Access;

private

   use Agpl.Types.Ustrings;

   type Object is new Root.Object with record
      Index  : Positive := 1;
      Prefix : Ustring
   end record;

end Sancta.Plugin.Skeleton;
