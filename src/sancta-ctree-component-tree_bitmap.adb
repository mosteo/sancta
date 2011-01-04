with Sancta.Ctree.CTTypes,
     Sancta.Ctree.Tree_Navigator.Bitmap,
     Sancta.Component.Factory,
     Sancta.Component.Types;

package body Sancta.Ctree.Component.Tree_Bitmap is

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
      Next :          Ada.Calendar.Time;
   begin
      This.Subscribe (Requires_Must_Replan);
      This.Run (Next);
      return Sancta.Component.Object_Access (This);
   end Create;

   ------------
   -- Replan --
   ------------

   procedure Replan (This : in out Object) is
      use Ctree.Tree_Navigator.Bitmap;
      Creator : constant Ctree.Tree_Navigator.Bitmap.Creators :=
                  Ctree.Tree_Navigator.Bitmap.Creators'Value
                    (This.Option (Option_Creator, "invalid"));
   begin
      if
        This.Exists (Requires_Map) and then
        This.Exists (Requires_Root_Pose) and then
        This.Exists (Requires_Tasks)
      then
         This.Done := True;
         Log ("Creating Tree...", Debug, Log_Section);

         This.Output
           (Provides_Tree,
            CTTypes.Tree_Nav'
              (Data with
               Ctree.Tree_Navigator.Handle.Set
                 (Ctree.Tree_Navigator.Bitmap.Available_Creators (Creator)
                  (Sancta.Component.Types.Pose
                     (This.Input (Requires_Root_Pose)).Pose,
                     Sancta.Component.Types.Task_List
                       (This.Input (Requires_Tasks)).Tasks,
                     Sancta.Component.Types.Map_Data
                       (This.Input (Requires_Map)).Map.Ref.all))));
      else
         Log ("Waiting for missing input", Debug, Log_Section);
      end if;
   end Replan;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      if not This.Done then
         Next := Clock + 0.01;
         This.Replan;
      else
         Next := Clock + 1020.01;
      end if;
   end Run;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
      use Sancta.Component.Types;
   begin
      if Bool (Value).Value then
         Log ("TREE REPLANNING", Always);
         This.Replan;
      end if;
   end Key_Stored;

end Sancta.Ctree.Component.Tree_Bitmap;
