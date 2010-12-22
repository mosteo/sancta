with Sancta.Assignment,
     Sancta.Component.Root,
     Sancta.Containers,
     Sancta.Map;
use Sancta,
    Sancta.Containers;

package Sancta.Component.Map_Tasks is

   --  Create random tasks in a map.
   --  They're created the first time the map is obtained.

   Name                 : aliased constant Component_Name := "map_tasks";

   Log_Section          : constant String := "sancta.component.map_tasks";

   Option_Amount        : constant Option_Attr := "amount";
   --  Natural saying amount of random tasks to create
   Option_Random_Seed   : constant Option_Attr := "random_seed";

   Requires_Map         : constant Internal_Key := "map";
   Requires_Team        : constant Internal_Key := "team";
   --  Used to remove candidate locations (optional)
   Provides_Tasks       : constant Internal_Key := "tasks";

   procedure Register;

   --  Direct use tools:

   function Create_Tasks (Amount : Natural;
                          Map    : Sancta.Map.Object'Class;
                          Team   : Assignment.Object;
                          Seed   : Integer := 0)
                          return Tc.Lists.List;

private

   type Object is new Root.Object with record
      Created : Boolean := False;
   end record;

   type Object_Access is access all Object;

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   not overriding
   procedure Create_Tasks (This : Object);

end Sancta.Component.Map_Tasks;
