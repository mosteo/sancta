--  Sample empty component to save-as when creating new ones.

with Sancta.Component.Root;

package Sancta.Component.Port_Waiter is

   --  Block at create until a port is opened, or provide a flag
   --  when it happens.

   Log_Section : constant String := "sancta.component.port_waiter";

   Name : aliased constant Component_Name := "port_waiter";

   Provides_Flag : constant Internal_Key := "flag";
   --  Bool

   Opt_Host : constant Option_Attr := "host";
   Def_Host : constant String      := "127.0.0.1";
   Opt_Port : constant Option_Attr := "port";
   Opt_Wait : constant Option_Attr := "wait";
   Def_Wait : constant Boolean     := True;
   --  If true, wait at create.
   --  The output flag is provided in all cases

   procedure Register;

private

   type Object is new Root.Object with null record;

   function Create (Config : Comp_Config)
                    return   Component.Object_Access;

end Sancta.Component.Port_Waiter;
