with Agpl.Containers.String_String_Maps,
     Sancta.Agent,
     Sancta.Assignment,
     Sancta.Containers,
     Sancta.Located_Agent;

use Sancta,
    Sancta.Containers;

package Sancta.Ctree.Team_Tree is

   --  Assignment extended with parent-child relations

   type Object is new Assignment.Object with private;

   function Create (From : Assignment.Object) return Object'Class;

   overriding
   function Create (Agent : Sancta.Agent.Object'Class) return Object;

   overriding
   function Create (Agents : AC.Lists.List)
                    return   Object;

   not overriding
   function Parent (This : Object;
                    Bot  : String) return Located_Agent.Object'Class;
   --  Itself for root.

   not overriding
   function Children (This : Object;
                      Bot  : String) return Ac.Lists.List;

   not overriding
   procedure Set_Parent (This   : in out Object;
                         Child  :        String;
                         Parent :        String);

private

   type Object is new Assignment.Object with record
      Parents : Agpl.Containers.String_String_Maps.Map;
   end record;

end Sancta.Ctree.Team_Tree;
