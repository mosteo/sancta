with Agpl.Drawing;
with Agpl.Generic_Handle;
with Sancta.Containers; use Sancta.Containers;
with Sancta.Map,
     Sancta.Tasks;

use Sancta;

package Sancta.Ctree.Tree_Navigator is

   type Preobject is abstract tagged null record;
   --  Needed to workaround a bug in GPL2008

   type Object is abstract new Preobject and Agpl.Drawing.Drawable with null record;

   --  This object encapsulates paths from base to a goal

   type Object_Access is access all Object'Class;

   not overriding
   function Branch (This : Object;
                    Goal : Tasks.Object'Class)
                    return Map.Path is abstract;

   not overriding
   function Get_Tasks (This : Object) return Tc.Lists.List is abstract;

   overriding
   procedure Draw (This :        Object;
                   Dest : in out Agpl.Drawing.Drawer'Class);

   package Handle is new Agpl.Generic_Handle (Object'Class);

   procedure Draw (This :        Object'Class;
                   Dest : in out Agpl.Drawing.Drawer'Class);

private

   procedure Draw_Classwide
     (This :        Object'Class;
      Dest : in out Agpl.Drawing.Drawer'Class) renames Draw;
   --  For use in body without ambiguity

end Sancta.Ctree.Tree_Navigator;
