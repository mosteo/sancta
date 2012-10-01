with Agpl.Tasking.Code,
     Agpl.Tasking.Workers,
     Sancta.Ctree.Component.Nctypes,
     Sancta.Ctree.Connectivity_Matrix,
     Sancta.Ctree.Draw,
     Sancta.Component.Ctypes,
     Sancta.Component.Factory,
     Sancta.Component.Player_Graphics2d,
     Sancta.Containers;

use Sancta.Containers;

--  with Agpl.Trace; use Agpl.Trace;

package body Sancta.Ctree.Component.Scluster_Draw is

   type Object_Access is access all Object;

   type Draw_Code (Parent : Object_Access)
     is new Agpl.Tasking.Code.Object with
      record
         Agents       : Ac.Lists.List;
         Tasks        : Tc.Lists.List;
         Real_Links,
         Scluster_Links : Connectivity_Matrix.Object;
      end record;

   overriding
   procedure Run (Thix : in out Draw_Code);

   ---------
   -- Run --
   ---------

   procedure Run (Thix : in out Draw_Code) is
      use Ada.Calendar,
          Player_Graphics2d;
      This : Object renames Thix.Parent.all;
   begin
      This.Output (Provides_Queue_Mesh,
                   Action_Clear'(Actions with null record));
      This.Output (Provides_Queue_Tasks,
                   Action_Clear'(Actions with null record));

      This.Output
        (Provides_Queue_Tasks,
         Action_Goals'
           (Actions with Thix.Tasks));

      This.Output
        (Provides_Queue_Tasks,
         Action_Assignment'
           (Actions with Thix.Agents));

      This.Output
        (Provides_Queue_Mesh,
         Draw.Action_Links_Alone'
           (Actions with
            Thix.Agents,
            Real_Links     => Thix.Real_Links,
            Scluster_Links => Thix.Scluster_Links.Spanning_Tree));

      This.Output (Provides_Queue_Mesh,
                   Action_Flush'(Actions with null record));
      This.Output (Provides_Queue_Tasks,
                   Action_Flush'(Actions with null record));

      This.Busy := False;
   end Run;

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
      if This.Exists (Option_Period) then
         This.Period := Duration'Value (This.Option (Option_Period));
      end if;

      return Sancta.Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      Next := Clock + This.Period;
      if not This.Busy and then This.Exists (Requires_All) then
         This.Do_Draw;
      end if;
   end Run;

   -------------
   -- Do_Draw --
   -------------

   procedure Do_Draw (This : in out Object) is
      C : constant Draw_Code :=
            (Parent => This'Unrestricted_Access,
             Agents => Ctypes.Teams
               (This.Input (Requires_Team)).Team.Get_Agents,
             Tasks  => Ctypes.Task_List
               (This.Input (Requires_Tasks)).Tasks,
             Real_Links => Nctypes.Links
               (This.Input (Requires_Real_Links)).Links,
             Scluster_Links => Nctypes.Links
               (This.Input (Requires_Scluster_Links)).Links);
   begin
      This.Busy := True;
      Agpl.Tasking.Workers.Launch (C,
                                   Stack => 4 * 1024 * 1024);
   end Do_Draw;

end Sancta.Ctree.Component.Scluster_Draw;
