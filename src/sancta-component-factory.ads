with Agpl.Chronos;
with Agpl.Tasking.Period;
with Agpl.Ustrings; use Agpl.Ustrings;
with Sancta.Component.Environment;
with Sancta.Component.Root;

--  Use this package to get instances of registered plugins

package Sancta.Component.Factory is

   Log_Section : constant String := "sancta.component.factory";

   Unregistered_Plugin : exception;

   type Object_Creator is access
     function (Config : in Comp_Config) return Object_Access;
   --  For a factory approach
   --  Config points to the component configuration.

   type Object_Creator_Plus is access
     function (Config : Comp_Config;
               Env    : Environment.Object) return Object_Access;

   pragma MUST_DIE;
   --  The above functions as creators are a gross mistake.
   --  Must be replaced with some class, which allows passing parameters
   --  to the creator before component creation.
   --  Ideally, a generic that's quickly instantiable would be... ideal?
   --  Also perhaps auto-registration... instead of explicit...
   --  But I moved away from that and I don't remember why... :'''(((

   function Create (Name   : Component_Name;
                    Config : Agpl.Xml.Node;
                    Env    : Environment.Object) return Object_Access;
   --  May raise Unregistered_Plugin

   procedure Register (Name    : Component_Name;
                       Creator : Object_Creator);

   procedure Register (Name    : Component_Name;
                       Creator : Object_Creator_Plus);
   --  Use whichever you need; most components need only the simplest one

   --  Helper for delayed creation of components.
   --  A component registered this way will only be created once all its
   --    required inputs exist in database.
   procedure Register (Name     : Component_Name;
                       Creator  : Object_Creator_Plus;
                       Required : Internal_Key_Array);
   --  This apparently works, but in the case of URUS, I get some (unrelated?)
   --  task deaths in ABNORMAL state... so to hell with it, I lost two days
   --  trying to get it running

private

   type Context_On_Demand is record
      Name : Ustring;
      Real : Object_Creator_Plus;
      Keys : Internal_Key_Vector;
   end record;

   type Creator_On_Demand is new Root.Object with record
      Context : Context_On_Demand;
      Env     : Environment.Object;
      Timer   : Agpl.Chronos.Object;
      Epoch   : Agpl.Chronos.Object;
      Period  : Agpl.Tasking.Period.Object;
   end record;

   type On_Demand_Access is access all Creator_On_Demand;

   overriding
   procedure Key_Stored (This  : in out Creator_On_Demand;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   overriding
   procedure Run (This : in out Creator_On_Demand;
                  Next :    out Ada.Calendar.Time);

   overriding
   function Requires_Mutex (This : Creator_On_Demand) return Boolean;

   overriding
   function Is_Master (This : Creator_On_Demand) return Boolean;

end Sancta.Component.Factory;
