--  This object tracks the status of given tasks, without taking into
--  consideration HTN related aspects like expansion, etc.

with Sancta.Plan;
with Sancta.Tasks.Containers;

package Sancta.Mission_Tracker is

   --   pragma Elaborate_body;

   Log_Section : constant String := "sancta.mission_tracker";

   type Object is tagged private;
   type Object_Access is access all Object'Class;

   procedure Add_Task (This : in out Object;
                       Job  : in     Sancta.Tasks.Object'Class);
   --  Will check that the task isn't already tracked

   procedure Add_Tasks (This : in out Object;
                        Jobs : in     Sancta.Tasks.Containers.Lists.List);

   procedure Clear (This : in out Object);
   --  Remove all tasks.

   function Contains (This : in Object;
                      Job  : in Sancta.Tasks.Task_Id) return Boolean;
   --  Checks both pending and finished tasks.

   function Get_Plan (This : in Object) return Sancta.Plan.Object;
   --  Tasks are properly tagged with Owner/Finished status, etc...

   procedure Mark_Finished (This : in out Object;
                            Id   : in     Sancta.Tasks.Task_Id);

   procedure Parse_File (This : in out Object;
                         Name : in     String);
   --  Get tasks from a file

   procedure Print_Summary (This : in Object);
   --  Dump to stdout

private

   type Object is tagged record
      Plan : Sancta.Plan.Object;
   end record;

end Sancta.Mission_Tracker;
