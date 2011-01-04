with Agpl.Tasking.Code,
     Agpl.Tasking.Workers,
     Sancta.Ctree.CTTypes,
     Sancta.Ctree.Connectivity_Matrix.Utils,
     Sancta.Ctree.Path_Trees,
     Sancta.Ctree.Team_Tree,
     Sancta.Ctree.Tree_Navigator,
     Sancta.Ctree.Tree_Navigator.Bitmap,
     Sancta.Ctree.Utils,
     Sancta.Ctree.Draw,
     Sancta.Component.Factory,
     Sancta.Component.Player_Graphics2d,
     Sancta.Component.Types,
     Sancta.Component.Utils,
     Sancta.Map.Smart,
     Sancta.Tasks;

package body Sancta.Ctree.Component.Ctree_Draw is

   package Pg2d renames Sancta.Component.Player_Graphics2d;

   use type Tasks.Object'Class;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Sancta.Component.Factory.Register (Name, Create'Access);
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
      This.Period := Sancta.Component.Utils.Option (This.all, "period", This.Period);
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
          Sancta.Component.Player_Graphics2d,
          Draw;
   begin
      Next := Clock + This.Period;
      if This.Exists (Requires_All) then
         declare
            Tasks : constant Tc.Lists.List :=
                      Sancta.Component.Types.Task_List
                        (This.Input (Requires_Tasks)).Tasks;

            Links : constant Connectivity_Matrix.Object :=
                      CTTypes.Links (This.Input (Requires_Links)).Links;

            Team_Tree : constant Ctree.Team_Tree.Object :=
                          CTTypes.Team_Tree
                            (This.Input (Requires_Team_Tree)).Team;

            Pruned_Links : constant Connectivity_Matrix.Object :=
                             Connectivity_Matrix.Utils.Keep_Only_Tree
                               (Links, Team_Tree);

            function Changes return Boolean is
               use type Tc.Lists.List;
            begin
               if This.Busy then
                  return True; pragma Wrong ("COULD THIS BE WRONG?");
               elsif This.Curr_Tasks /= Tasks then
                  return True;
               elsif not This.Links.Same_Topology (Pruned_Links) then
                  return True;
               end if;

               return False;
            end Changes;
         begin
            if Changes or else This.First_Time then
               This.Do_Draw;
            end if;
         end;
      end if;
   end Run;

   type Draw_Code (Parent : Object_Access)
     is new Agpl.Tasking.Code.Object with
      record
         Tree  : Ctree.Tree_Navigator.Bitmap.Object;
         Tasks : Tc.Lists.List;
         Links, Pruned_Links : Connectivity_Matrix.Object;
         Team_Tree : Ctree.Team_Tree.Object;
      end record;

   overriding
   procedure Run (Thix : in out Draw_Code);

   ---------
   -- Run --
   ---------

   procedure Run (Thix : in out Draw_Code) is
      use Ada.Calendar,
          Sancta.Component.Player_Graphics2d,
          Draw;
      This : Object renames Thix.Parent.all;
      M    : constant Map.Smart.Object :=
               Sancta.Component.Types.Map_Data (This.Input (Requires_Map)).Map;

      procedure Pass_Tree (I : Ctree.Path_Trees.Cursor) is
         P : Map.Path;
         use Ctree.Utils;
      begin
         if not I.Is_Root then
            P.Append (Node_Data (I.Parent.Query.all).Loc.Get);
            P.Append (Node_Data (I.Query.all).Loc.Get);
            This.Output
              (Provides_Queue_Tasks,
               Action_Path'
                 (Actions with M, P, (0, 100, 200, 200)));
         end if;
      end Pass_Tree;

   begin
      This.First_Time := False;
      Log ("Changes, redrawing...", Debug, Log_Section);
      This.Links := Thix.Pruned_Links;

      This.Output (Provides_Queue_Mesh,
                   Action_Clear'(Actions with null record));
      This.Output (Provides_Queue_Tasks,
                   Action_Clear'(Actions with null record));

      --  Map
      if M.Ref.all in Pg2d.Player_Drawable'Class then
         declare
            Acts : Pg2d.Action_Vector;
         begin
            Pg2d.Player_Drawable'Class (M.Ref.all).Player_Draw (Acts);
            for I in Acts.First_Index .. Acts.Last_Index loop
               This.Output (Provides_Queue_Mesh, Acts.Element (I));
            end loop;
         end;
      end if;

      --  Tasks
      This.Output
        (Provides_Queue_Tasks,
         Action_Goals'
           (Actions with Thix.Tasks));

      declare
         T : Ctree.Path_Trees.Tree;
      begin
         Ctree.Utils.Build_Tree (T, Thix.Tree, Thix.Tasks);
         T.Iterate (Pass_Tree'Access);
      end;

      --  Thix.Tasks.Iterate (Pass_Task_Branches'Access);

      if not This.Curr_Tasks.Is_Empty then
         if
         not Thix.Tasks.Is_Empty and then
           Thix.Tasks.First_Element /= This.Curr_Tasks.First_Element
         then
            This.Prev_Branch := This.Curr_Branch;
            This.Curr_Branch := Thix.Tree.Branch (Thix.Tasks.First_Element);
            This.Prev_Tasks := This.Curr_Tasks;
            This.Curr_Tasks := Thix.Tasks;
         elsif Thix.Tasks.Is_Empty then
            This.Prev_Branch := This.Curr_Branch;
         end if;
      elsif not Thix.Tasks.Is_Empty then
         This.Curr_Tasks := Thix.Tasks;
         This.Curr_Branch := Thix.Tree.Branch (Thix.Tasks.First_Element);
      end if;

      This.Output
        (Provides_Queue_Tasks,
         Action_Path'
           (Actions with
            Sancta.Component.Types.Map_Data (This.Input (Requires_Map)).Map,
            This.Prev_Branch,
            (0, 200, 100, 100)));

      This.Output
        (Provides_Queue_Mesh,
         Draw.Action_Links'
           (Actions with Thix.Links, Thix.Team_Tree));

      This.Output (Provides_Queue_Tasks, Do_Action_Flush);
      This.Output (Provides_Queue_Mesh,  Do_Action_Flush);

      This.Busy := False;
   end Run;

   -------------
   -- Do_Draw --
   -------------

   procedure Do_Draw (This : in out Object) is
      C : Draw_Code
        (Parent => This'Unrestricted_Access);
   begin
      C.Tree := Ctree.Tree_Navigator.Bitmap.Object
        (CTTypes.Tree_Nav (This.Input (Requires_Tree)).Tree.Ref.all);
      C.Tasks := Sancta.Component.Types.Task_List
        (This.Input (Requires_Tasks)).Tasks;
      C.Links :=  CTTypes.Links (This.Input (Requires_Links)).Links;
      C.Team_Tree := CTTypes.Team_Tree
        (This.Input (Requires_Team_Tree)).Team;
      C.Pruned_Links :=  Connectivity_Matrix.Utils.Keep_Only_Tree
        (C.Links, C.Team_Tree);
      This.Busy := True;
      Agpl.Tasking.Workers.Launch (C,
                                   Stack => 4 * 1024 * 1024);
   end Do_Draw;

end Sancta.Ctree.Component.Ctree_Draw;
