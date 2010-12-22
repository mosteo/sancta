with Sancta.Component.One_Shot;

package Sancta.Component.Worst_Cost is

   --  Component base that may be used when processing must be performed
   --  only once, when the input keys are available for the first time.

   Log_Section : constant String := "sancta.component.worst_cost";

   Name : aliased constant Component_Name := "worst_cost";

   Requires_Map       : aliased constant Internal_Key := "map";
   Requires_From_Pose : aliased constant Internal_Key := "from_pose";

   type Object is new One_Shot.Object with null record;

   overriding
   procedure Process (This : in out Object);
   --  Override and do whatever (generating Output).
   --  This is called only when all the inputs are available.

   overriding
   function Inputs (This : Object) return Internal_Key_Array;
   --  Inputs to be monitored; they're automatically subscribed so you don't
   --  need to do so.

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

end Sancta.Component.Worst_Cost;
