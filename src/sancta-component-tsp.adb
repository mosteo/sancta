with Sancta.Agent.Utils,
     Sancta.Assigner.Mtsp_Concorde,
     Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Component.Utils,
     Sancta.Containers,
     Sancta.Cost_Matrix;

use Sancta.Containers;

package body Sancta.Component.Tsp is

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
     (Config : Agpl.Xml.Node)
      return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Subscribe (Requires_Must_Replan);
      This.Subscribe (Requires_Agent);
      This.Subscribe (Requires_Tasks);

      This.Replan;

      return Component.Object_Access (This);
   end Create;

   ------------
   -- Replan --
   ------------

   procedure Replan (This : in out Object) is
      use Utils;
   begin
      if This.Exists (Requires_Agent) and then This.Exists (Requires_Tasks) then
         declare
            use Agent.Utils;
            Bot  : constant Agent.Object'Class :=
              Types.Agent (This.Input (Requires_Agent)).Agent.all;
            Jobs : constant Tc.Lists.List :=
              Types.Task_List (This.Input (Requires_Tasks)).Tasks;
            Cm   :          Cost_Matrix.Object :=
                     Cost_Matrix.Create_With_Start (+Bot, Jobs);
            Closed : constant Boolean := Option (This, Option_Closed, False);
         begin
            if Closed then
               Cm.Create_Closed_Costs (+Bot, Jobs);
            end if;
            declare
               Opt  : constant Tc.Lists.List :=
                        Assigner.Mtsp_Concorde.Assign
                          (+Bot,
                           Jobs,
                           Cm,
                           Closed).Get_All_Tasks;
            begin
               This.Output (Provides_Plan, Types.Task_List'(Data with Opt));
            end;
         end;
      end if;
   end Replan;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
   begin
      if Key = Requires_Must_Replan and then Types.Bool (Value).Value then
         This.Replan;
      elsif Key = Requires_Tasks then
         This.Output (Provides_Plan, Value);
      end if;
   end Key_Stored;

end Sancta.Component.Tsp;
