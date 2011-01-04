with Sancta.Agent_Proxy,
     Sancta.Map,
     Sancta.Map.Smart,
     Sancta.Tasks,
     Sancta.Types;
use Sancta;

package Sancta.Ctree.Robot is

   --  pragma Preelaborate;

   --  Globally set a map for use in cost computation
   --  Yes, horrid hack but...
   pragma Global_Variable ("The_Map");

   The_Map : Map.Smart.Object;

   function Get_Prefix return Map.Path;
   --  Another hellish patch

   type Object is new Agent_Proxy.Object with private;

   function Create (Name : String;
                    Pose : Types.Pose) return Object'Class;

   overriding
   function Get_Cost
     (This     : Object;
      Ini, Fin : Sancta.Tasks.Object'Class)
      return     Sancta.Costs;

   not overriding
   procedure Set_Mobile (This   : in out Object;
                         Mobile :        Boolean := True);

   not overriding
   function Is_Mobile (This : Object) return Boolean;

private

   type Object is new Agent_Proxy.Object with record
      Mobile : Boolean := True;
   end record;

end Sancta.Ctree.Robot;
