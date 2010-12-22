with Sancta.Distributed; pragma Elaborate_All (Sancta.Distributed);
limited with Sancta.Located_Agent;
with Sancta.Component.Root;

with Sancta.Mutable_Assignment;
with Sancta.Tasks;

with System;

package Sancta.Component.Annealer is

   Log_Section : constant String := "sancta.Component.annealer";
   Detail_Section : constant String := "sancta.Component.annealer.detail";

   Plugin_Name : constant String := "annealer";

   Death_Lateness : constant Duration := 5.0;

   Requires_Agent    : constant Data_Key := "agent";
   Requires_Database : constant Data_Key := "database";

   --  Shared objects of interest:
   use type Distributed.Object_Key;

   Shared_Context_Key
     : constant Distributed.Object_Key := +"global.anneal.context";
   --  Contains a Plan, its best known assignment and the resulting cost.
   --  Type: Danneal

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   not overriding
   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   --  Any of the following subprograms can be removed entirely if they're not
   --  going to be used, since there's null defaults in the Root class.

--     overriding
--     procedure Key_Stored
--       (This  : in out Object;
--        Key   : in     Agpl.Protected_Datastore.Object_Key;
--        Value : in     Agpl.Protected_Datastore.Object_Data'Class) is null;

--     overriding
--     procedure Run (This : in out Object;
--                    Next :    out Ada.Calendar.Time) is null;

   overriding
   procedure Stop (This : in out Object);

   --  UTILITIES  --

   procedure Add_Task (Job : in     Sancta.Tasks.Object'Class;
                       Ok  :    out Boolean);
   --  Add a task to the annealing context (will check that the component exists).

   procedure Set_Criterion (Criterion : in Sancta.Assignment_Criteria);
   --  Set the new criterion.

   procedure Toggle;
   --  Switch execution on/off.

private

   task type Annealer_Task (This : access Object) is
      pragma Priority (System.Priority'First);
      pragma Storage_Size (1024 * 1024);

      entry Start;
      entry Shutdown;
   end Annealer_Task;

   type Object is new Root.Object with record
      Annealer_Active : Annealer_Task (Object'Access);
      Anneal          : Sancta.Mutable_Assignment.Object;
      Criterion       : Sancta.Assignment_Criteria;
      Bot             : access Located_Agent.Object'Class;
      Execute         : Boolean;
      Log             : Boolean := False;
   end record;

end Sancta.Component.Annealer;
