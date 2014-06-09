with Sancta.Component.Root;

package Sancta.Component.Tsp is

   --  Solve on demand a TSP problem.
   --  When not requested, tasks are passed along as-is

   Log_Section : constant String := "sancta.component.tsp";

   Name : aliased constant Component_Name := "tsp";

   Option_Closed         : constant Option_Attr  := "closed";
   --  If Closed, return to starting city

   Requires_Must_Replan  : constant Internal_Key := "must_replan";
   Requires_Agent        : constant Internal_Key := "agent";
   --  Provides costs
   Requires_Tasks        : constant Internal_Key := "tasks";

   Provides_Plan         : constant Internal_Key := "plan";

   procedure Register;

private

   type Object is new Root.Object with null record;

   type Object_Access is access all Object'Class;

   function Create (Config : Agpl.Xml.Node)
                    return   Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   not overriding
   procedure Replan (This : in out Object);

end Sancta.Component.Tsp;
