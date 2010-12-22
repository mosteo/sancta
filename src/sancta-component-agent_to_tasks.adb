with Sancta.Component.Factory,
     Sancta.Component.Helper,
     Sancta.Component.Types;

package body Sancta.Component.Agent_To_Tasks is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
      Help : constant Helper.Object := Helper.Create (Config);
      This : constant Object_Access :=
               new Object (Name'Access,
                           Config,
                           Types.Agent (Help.Input (Requires_Agent)).Agent);
   begin
      if This.Exists (Option_Period) then
         This.Period := Duration'Value (This.Option (Option_Period, ""));
      end if;

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
      use Tc.Lists;
   begin
      Next := Clock + This.Period;
      declare
         Curr : constant Tc.Lists.List := This.Agent.Get_Tasks;
      begin
         if This.Agent.Get_Tasks /= This.Prev then
            This.Output (Provides_Tasks, Types.Task_List'(Tasks => Curr));
            This.Prev := Curr;
         end if;
      end;
   end Run;

end Sancta.Component.Agent_To_Tasks;
