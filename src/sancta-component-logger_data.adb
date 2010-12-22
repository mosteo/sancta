with Sancta.Component.Factory;

--  with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Text_Io; use Ada.Text_Io;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Logger_Data is

   use Ada.Calendar;

   ---------------
   -- Log_Value --
   ---------------

   procedure Log_Value (This  : in out Object;
                        Value : in     Agpl.Protected_Datastore.Object_Data'Class)
   is
      use type Agpl.Protected_Datastore.Object_Data'Class;
   begin
      if This.Remove_Duplicates and then
        This.Prev_Data.Is_Valid and then
        This.Prev_Data.Get = Value
      then
         return;
      elsif This.Remove_Duplicates then
         This.Prev_Data.Set (Value);
      end if;

      Put_Line (This.File, Image (Value));
      Flush (This.File);

      if This.Echo then
         Put_Line (Component_Name & ": " & Image (Value));
      end if;
   end Log_Value;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      use Agpl.Xml;
      This : constant Object_Access :=
               new Object (new String'(Component_Name), Config);
   begin
      This.Filename := new String'
        (This.Option (Option_Filename, "data.log"));

      This.Periodic := Boolean'Value
        (This.Option (Option_Periodic, "false"));

      This.Period := Duration'Value
        (This.Option (Option_Period, "0.1"));

      This.Remove_Duplicates := Boolean'Value
        (This.Option (Option_Remove_Duplicates, "true"));

      This.Echo := Boolean'Value
        (Get_Attribute (Config, "echo", "true"));

      --  Add ourselves as listeners:
      if not This.Periodic then
         This.Subscribe (Requires_Data);
      end if;

      Create (This.File, Out_File, This.Filename.all);

      return Component.Object_Access (This);
   end Create;

   -------------------
   -- On_Key_Stored --
   -------------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
   begin
      if not This.Periodic then
         Log_Value (This, Value);
      end if;
   end Key_Stored;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Time) is
   begin
      if This.Periodic then
         This.Next := This.Next + This.Period;
         Next      := This.Next;
         Log_Value (This, This.Input (Requires_Data));
      else
         Next := Clock + 10894.0;
      end if;
   end Run;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Object) is
   begin
      Close (This.File);
   end Stop;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Component_Name, Logger_Data.Create'Unrestricted_Access);
   end Register;

end Sancta.Component.Logger_Data;
