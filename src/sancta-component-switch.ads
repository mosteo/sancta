with Sancta.Component.Root;

package Sancta.Component.Switch is

   --  Choose between two inputs depending on a third key

   Name       : aliased constant Component_Name := "switch";

   Requires_Switcher : constant Internal_Key := "switcher";
   --  Don't exist: OFF
   --  Type Ctypes.Bool: (False => OFF, True => ON)
   --  not Ctypes.Bool and Exists: ON

   Requires_When_Off : constant Internal_Key := "when_off";
   Requires_When_On  : constant Internal_Key := "when_on";

   Provides_Output   : constant Internal_Key := "output";

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node)
                       return      Component.Object_Access;

   type States is (Off, On);

   type Object is new Root.Object with record
      Status : States := Off; pragma Atomic (Status);
   end record;

   type Object_Access is access all Object'Class;

   overriding
   function Is_Thread_Safe (This : Object) return Boolean;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Switch;
