 

with Sancta.Plan;

package Sancta.Controller_Observer is

   pragma Preelaborate;

   type Object is abstract tagged null record;
   --  This kind of object is used as a callback mechanism so the controller
   --  can notify some external entity of happenings.

   procedure Plan_Changed (This : in out Object; New_Plan : Sancta.Plan.Object)
   is abstract;
   --  Invoked when the current plan in the Observer has changed.

end Sancta.Controller_Observer;
