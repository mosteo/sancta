with Ada.Strings.Hash;

package body Sancta.Agent_Task_Key is

   ---------
   -- Get --
   ---------

   function Get
     (A : in Sancta.Agent.Object'Class;
      T : in Sancta.Tasks.Object'Class)
      return Object
   is
   begin
      return (Agent => +(A.Get_Name),
              Tid   => T.Get_Id);
   end Get;

   ----------
   -- Hash --
   ----------

   function Hash (This : in Object) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (+This.Agent & ":x:" & Sancta.Tasks.Task_Id'Image (This.Tid));
   end Hash;

end Sancta.Agent_Task_Key;
