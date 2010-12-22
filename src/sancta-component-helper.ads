with Sancta.Component.Root;

package Sancta.Component.Helper is

   --  This do-nothing component may be instantiated in order to access
   --    inputs/outputs before other component needing them is created.
   --  For instance, in the call to Create we may want to get some
   --    constraining types for a component being created.
   --  It is also useful to get inputs from a non-component (i.e. test programs)

   Name : aliased constant Component_Name := "helper";

   type Object is new Root.Object with null record;

   function Create (Config : in Comp_Config) return Object;

   procedure Register;

   --  These are a set of helper subprograms that will create a temporary helper
   --    to save the user from doing it:

   function Option (Config : Comp_Config;
                    Attr   : Option_Attr) return String;

   function Option (Config : Comp_Config;
                    Attr   : Option_Attr;
                    Def    : String) return String;

   function Input (Config : Comp_Config;
                   Key    : Internal_Key) return Data'Class;

private

   function Create (Config : in Comp_Config) return Component.Object_Access;

end Sancta.Component.Helper;
