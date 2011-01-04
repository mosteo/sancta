with Sancta.Ctree.Utils,
     Sancta.Ctree.CTTypes,
     Sancta.Component.Factory,
     Sancta.Component.Types;

package body Sancta.Ctree.Component.Ctree_Tasks_Order is

   ------------
   -- Inputs --
   ------------

   function Inputs (This : Object) return Internal_Key_Array is
      pragma Unreferenced (This);
   begin
      return (1 => Requires_Tasks'Access,
              2 => Requires_Nav_Tree'Access);
   end Inputs;

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
      return Sancta.Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      for I in This.Inputs'Range loop
         This.Subscribe (This.Inputs (I).all);
      end loop;
      This.Subscribe (Requires_Must_Replan);
      This.Process;
      return Sancta.Component.Object_Access (This);
   end Create;

   -------------
   -- Process --
   -------------

   procedure Process
     (This : in out Object)
   is
      Orderer : constant Ctree.Utils.Orderers := Ctree.Utils.Orderers'Value
        (This.Option (Option_Creator, ""));
   begin
      if This.Exists (This.Inputs) then
         This.Done := True;
         Log ("Computing order...", Debug, Log_Section);
         This.Output
           (Provides_Tasks,
            Sancta.Component.Types.Task_List'
              (Data with
               Ctree.Utils.Available_Orderers (Orderer)
               (CTTypes.Tree_Nav
                  (This.Input (Requires_Nav_Tree)).Tree.Ref.all,
                Sancta.Component.Types.Task_List
                    (This.Input (Requires_Tasks)).Tasks)));
      else
         Log ("Waiting for missing data", Debug, Log_Section);
      end if;
   end Process;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      if This.Done then
         Next := Clock + 1000.0;
      else
         Next := Clock + 0.01;
         This.Process;
      end if;
   end Run;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      use Sancta.Component.Types;
   begin
      if Key = Requires_Must_Replan and then Bool (Value).Value then
         This.Process;
      end if;
   end Key_Stored;

end Sancta.Ctree.Component.Ctree_Tasks_Order;
