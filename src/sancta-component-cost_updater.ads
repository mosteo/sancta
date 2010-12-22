with Agpl.Tasking.Period,
     Sancta.Agent,
     Sancta.Component.Root,
     Sancta.Containers,
     Sancta.Cost_Cache;

package Sancta.Component.Cost_Updater is

   --  Updates costs from current agent location to its list of tasks.
   --    And, optionally, to a supplied list of tasks

   Log_Section : constant String := "sancta.component.cost_updater";

   Name : aliased constant Component_Name := "cost_updater";

   --  <component name="cost_updater" period="1.0">
   --     <requires data="agent" as="agent" type="agent" />
   --     <requires data="cost"  as="cost"  type="cost_cache" />
   --  </component>

   Requires_Agent : constant Internal_Key := "agent";
   --  Either agent or team may exist, but are not mandatory.
   Requires_Team  : constant Internal_Key := "team";

   Requires_Cost  : constant Internal_Key := "cost";
   Requires_Tasks : constant Internal_Key := "tasks";
   --  This is only needed if "use_tasks" below is true.

   Option_Period     : constant Option_Attr  := "period";
   Default_Period    : constant Duration     := 1.0;

   Option_Use_Tasks  : constant Option_Attr  := "use_tasks";
   Default_Use_Tasks : constant Boolean      := False;

   Option_Create_All  : constant Option_Attr  := "create_all";
   Default_Create_All : constant Boolean      := False;
   --  If this is true, the full matrix of costs will be created each time that
   --   Requires_Tasks changes.


   type Object (<>) is new Root.Object with private;

   procedure Register;

private

   use Agpl,
       Sancta.Containers;

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Cost   : access          Cost_Cache.Object'Class) is
     new Root.Object (Name, Config) with
      record
         Period             : Tasking.Period.Object :=
                                Tasking.Period.Create (Default_Period);
         Use_External_Tasks : Boolean  := Default_Use_Tasks;
         Create_All         : Boolean  := Default_Create_All;

         Agent              : Sancta.Agent.Object_Access;
         Tasks              : Tc.Lists.List;
      end record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   not overriding
   procedure Do_Update (This : in out Object);

end Sancta.Component.Cost_Updater;
