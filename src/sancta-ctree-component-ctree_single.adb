with Sancta.Ctree.CTTypes,
     Sancta,
     Sancta.Assignment,
     Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Component.Utils,
     Sancta.Containers,
     Sancta.Map;

use Sancta.Containers;

package body Sancta.Ctree.Component.Ctree_Single is

   use type Tc.Lists.List;

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
      Next :          Ada.Calendar.Time;
   begin
      This.Period := Utils.Option (This.all, Option_Period, 0.02);
      This.Run (Next);
      return Sancta.Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar,
          CTTypes,
          Sancta.Component.Types;
      use type Ctree.Single_Mover.Object_Access;
   begin
      if This.Mover = null and then This.Exists (Requires_To_Create) then
         declare
            M : Map.Object'Class renames
              Map_Data (This.Input (Requires_Map)).Map.Ref.all;
            pragma Unreferenced (M);
         begin
            This.Mover := new Ctree.Single_Mover.Object'
              (Ctree.Single_Mover.Create
                 (Teams     (This.Input (Requires_Team)).Team,
                  Task_List (This.Input (Requires_Plan)).Tasks,
--                    Tree_Nav  (This.Input (Requires_Tree)).This.all,
--                    M.Nearest_Location
--                      (Pose   (This.Input (Requires_Root_Pose)).Pose),
                  Utils.Option (This, Option_Here, Sancta.Costs'(1.0)),
                  Utils.Option (This, Option_Near, Sancta.Costs'(2.0)),
                  Map_Data  (This.Input (Requires_Map)).Map));
            Log ("Created.", Debug, Log_Section);
         end;
      elsif This.Mover /= null and then This.Exists (Requires_To_Run) then
         declare
            Event : Ctree.Single_Mover.Step_Outputs;
            Team  : Assignment.Object;
            use Ctree.Single_Mover;
            Replan : Boolean := False;
         begin
            Log ("Step", Debug, Log_Section);
            This.Mover.Step
              (Teams     (This.Input (Requires_Team)).Team,
               Task_List (This.Input (Requires_Plan)).Tasks,
               Tree_Nav  (This.Input (Requires_Tree)).Tree.Ref.all,
               Links     (This.Input (Requires_Links)).Links,
               Team,
               Event);

            if Event = Mission_Completed then
               Log ("Fallbacks:" & This.Fallbacks'Img, Always, Log_Section);
            end if;

            This.Output (Provides_Flag,
                         Bool'(CTTypes.Data with Event = Mission_Completed));
            This.Output (Provides_Team_Action,
                         Teams'(CTTypes.Data with Team));

            if
              (not This.Exists (Provides_Pending_Tasks)) or else
              Task_List (This.Input (Provides_Pending_Tasks)).Tasks /=
              This.Mover.Pending_Tasks
            then
               This.Output (Provides_Pending_Tasks,
                            Sancta.Component.Types.Task_List'
                              (CTTypes.Data with This.Mover.Pending_Tasks));
            end if;

            This.Output (Provides_Team_Tree,
                         CTTypes.Team_Tree'
                           (CTTypes.Data with This.Mover.Team_Tree));

            if Event = Head_Strained then
               if not This.Replan_Triggered then
                  This.Fallbacks := This.Fallbacks + 1;
               end if;
               Replan := not This.Replan_Triggered;
               This.Replan_Triggered := True;
            else
               This.Replan_Triggered := False;
            end if;

            This.Output
              (Provides_Must_Replan,
               Sancta.Component.Types.Bool'(Sancta.Component.Data with Replan));
         end;
      elsif not This.Exists (Requires_To_Create) then
         Log ("Create requisites missing", Debug, Log_Section);
      elsif not This.Exists (Requires_To_Run) then
         Log ("Run requisites missing", Debug, Log_Section);
      else
         Log ("Unknown requisites missing (?)", Debug, Log_Section);
      end if;

      loop
         This.Next := This.Next + This.Period;
         exit when This.Next >= Clock;
      end loop;

      Next := This.Next;
   end Run;

end Sancta.Ctree.Component.Ctree_Single;
