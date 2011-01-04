with Sancta.Plans,
     Sancta.Tasks.Handle;

use Sancta;

package Sancta.Ctree.Urus.Plan_Node is

   pragma Preelaborate;

   type Object is new Plans.Node with record
      Job : Tasks.Handle.Object;
   end record;

end Sancta.Ctree.Urus.Plan_Node;
