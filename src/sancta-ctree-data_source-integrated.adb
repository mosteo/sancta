with Sancta.Ctree.Connectivity_Matrix.Utils;
with Sancta.Ctree.Data_Server.Utils;

package body Sancta.Ctree.Data_Source.Integrated is

   ---------------
   -- Get_Links --
   ---------------

   function Get_Links
     (This : Object;
      Bots : Natural)
      return Connectivity_Matrix.Object
   is
      pragma Unreferenced (This);
   begin
      return Connectivity_Matrix.Utils.From_Datastore (Bots);
   end Get_Links;

   ---------------
   -- Get_Poses --
   ---------------

   function Get_Poses
     (This : Object;
      Bots : Natural)
      return Sancta.Assignment.Object
   is
      pragma Unreferenced (This);
      Ass : Sancta.Assignment.Object;
   begin
      Data_Server.Utils.Get_Poses (Bots, Ass);
      return Ass;
   end Get_Poses;

   ---------------
   -- Set_Goals --
   ---------------

   procedure Set_Goals
     (This : Object;
      Bots : Natural;
      Ass  : Sancta.Assignment.Object)
   is
      pragma Unreferenced (This);
   begin
      Data_Server.Utils.Set_Goals (Bots, Ass);
   end Set_Goals;

   -------------------
   -- Receive_Goals --
   -------------------

   function Receive_Goals (This : Object) return Sancta.Assignment.Object is
   begin
      pragma Unreferenced (This);
      return Sancta.Assignment.Object (Sancta.Assignment.Empty_Object);
   end Receive_Goals;

begin
   Register (Sancta.Ctree.Integrated, Object'(Data_Source.Object with null record));
end Sancta.Ctree.Data_Source.Integrated;
