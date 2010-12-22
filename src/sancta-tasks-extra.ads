with Sancta.Tasks.Containers;
with Sancta.Tasks;

package Sancta.Tasks.Extra is

   pragma Preelaborate;

   Log_Section : constant String := "sancta.tasks.extra";

   function Contains (T  : Sancta.Tasks.Containers.Lists.List;
                      Id : Sancta.Tasks.Task_Id)
                      return Boolean;

   procedure Insert (List : in out Containers.Lists.List;
                     Job  :        Object'Class;
                     Bfr  :        Task_Id := No_Task);

   procedure Print (List : Containers.Lists.List);

end Sancta.Tasks.Extra;
