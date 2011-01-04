with Sancta.Ctree.Utils,
     Sancta.Component.Types,
     Sancta.Component.Factory,
     Sancta.Ctree.Ctypes;

package body Sancta.Ctree.Component.Ctree_Preorder is

   ------------
   -- Inputs --
   ------------

   function Inputs (This : Object) return Internal_Key_Array is
      pragma Unreferenced (This);
   begin
      return (1 => Requires_Tasks'Access,
              2 => Requires_Tree'Access);
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
      return Sancta.Component.Object_Access (This);
   end Create;

   -------------
   -- Process --
   -------------

   procedure Process
     (This : in out Object)
   is
   begin
      Log ("Generating preorder...", Debug, Log_Section);
      This.Output
        (Provides_Tasks,
         Sancta.Component.Types.Task_List'
           (Data with
            Ctree.Utils.Preorder
              (CTypes.Tree_Nav (This.Input (Requires_Tree)).Tree.Ref.all,
               Sancta.Component.Types.Task_List (This.Input (Requires_Tasks)).tasks)));
   end Process;

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      pragma Unreferenced (Value);
   begin
      if Key = Requires_Tasks and then This.Exists (Requires_Tree) then
         This.Process;
      end if;
   end Key_Stored;

end Sancta.Ctree.Component.Ctree_Preorder;
