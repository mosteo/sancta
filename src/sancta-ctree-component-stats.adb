with Sancta.Ctree.Component.Nctypes,
     Sancta.Component.Ctypes,
     Sancta.Component.Factory;

package body Sancta.Ctree.Component.Stats is

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
     (Config : Comp_Config)
      return Sancta.Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Subscribe (Requires_Pending_Tasks);
      This.Subscribe (Requires_Team);
      This.Subscribe (Requires_Links);

      --  Trigger initialization
      if This.Exists (Requires_Pending_Tasks) then
         This.Key_Stored (Requires_Pending_Tasks,
                          This.Input (Requires_Pending_Tasks));
      end if;

      return Sancta.Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
   begin
      if not This.Inited then
         if Key = Requires_Links then
            This.Inited := True;
            This.Tracker.Init (Nctypes.Links (Value).Links);
            Log ("Stat tracker initialized", Debug, Log_Section);
         end if;
      else
         if Key = Requires_Pending_Tasks then
            declare
               Pending_Tasks : constant Tc.Lists.List :=
                                 Ctypes.Task_List (Value).Tasks;
            begin
               if This.Tasks.Is_Empty then
                  This.Tasks := Pending_Tasks;
                  Log ("Stat tracker tasks initialized", Debug, Log_Section);
               else
                  for I in 1 .. Natural (This.Tasks.Length) -
                                Natural (Pending_Tasks.Length)
                  loop
                     This.Tracker.Mark_Task_Completed;
                     Log ("Marking task completed", Debug, Log_Section);
                  end loop;
                  This.Tasks := Pending_Tasks;
               end if;

               if Pending_Tasks.Is_Empty then
                  This.Tracker.Print;
               end if;
            end;
         elsif Key = Requires_Team then
            if This.Exists (Requires_Links) then
               This.Tracker.Update
                 (Ctypes.Teams     (Value).Team,
                  Nctypes.Links    (This.Input (Requires_Links)).Links,
                  Ctypes.Task_List (This.Input (Requires_Pending_Tasks)).Tasks);
            end if;
         end if;
      end if;
   end Key_Stored;

   ----------
   -- Stop --
   ----------

   procedure Stop
     (This : in out Object)
   is
   begin
      This.Tracker.Print;
   end Stop;

end Sancta.Ctree.Component.Stats;
