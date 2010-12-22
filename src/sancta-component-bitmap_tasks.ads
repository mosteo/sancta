with Sancta.Assignment,
     Sancta.Component.Root,
     Sancta.Containers,
     Sancta.Map.Bitmap;
use Sancta,
    Sancta.Containers;

package Sancta.Component.Bitmap_Tasks is

   --  Create random tasks in a bitmap.
   --  They're created the first time the bitmap is obtained.

   Name                 : aliased constant Component_Name := "bitmap_tasks";

   Log_Section          : constant String := "sancta.component.bitmap_tasks";

   Option_Amount        : constant Option_Attr := "amount";
   --  Natural saying amount of random tasks to create
   Option_Random_Seed   : constant Option_Attr := "random_seed";

   --  Missing option for sigma below, unimplemented

   Requires_Map         : constant Internal_Key := "map";
   Requires_Team        : constant Internal_Key := "team";
   --  Used to remove candidate locations
   Provides_Tasks       : constant Internal_Key := "tasks";

   procedure Register;

   --  Direct use tools:

   subtype Sigma_Range is Float range 0.0 .. Float'Last;
   subtype Break_Range is Float range 0.0 .. 1.0;

   function Create_Tasks (Amount        : Natural;
                          Map           : Sancta.Map.Bitmap.Object'Class;
                          Team          : Assignment.Object;
                          Seed          : Integer := 0;
                          Cluster_Sigma : Sigma_Range := 0.0;
                          Cluster_Break : Break_Range := 0.9)
                          return Tc.Lists.List;
   --  Use a Sigma > 0 in order to probabilistically accept tasks.
   --  Remember that sigma is standard deviation.
   --  The condition is that a random value from the distro is greater than the
   --  distance between the new task and its closest previous task.
   --  Alternatively, if Rnd < cluster_break, task is accepted and a new cluster
   --  may appear.

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

end Sancta.Component.Bitmap_Tasks;
