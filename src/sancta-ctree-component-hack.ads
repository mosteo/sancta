with Ada.Calendar,
     Agpl.Xml,
     Sancta.Ctree.Robot,
     Sancta.Agent,
     Sancta.Component,
     Sancta.Component.Root;
use Sancta,
    Sancta.Component,
    Sancta.Component.Root;

package Sancta.Ctree.Component.Hack is

   --  Perform any initialization hacks necessary. Should dissapear eventually

   Name                 : aliased constant Component_Name := "nerus_hack";

   Log_Section          : constant String := "Sancta.Ctree.Component.hack";

   Requires_Map         : constant Internal_Key := "map";
   Requires_Base_Agent  : constant Internal_Key := "base_agent";

   Provides_Base  : constant Internal_Key := "nerus_base";

   type Object is new Root.Object with private;
   type Object_Access is access all Object;

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node)
                    return Sancta.Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   type Object is new Root.Object with record
      Map_Passed : Boolean := False;
      Bot        : Sancta.Agent.Object_Access := new Robot.Object;
   end record;

end Sancta.Ctree.Component.Hack;
