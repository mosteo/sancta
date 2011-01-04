package body Sancta.Ctree.Urus.Tasks.Compound is

   --------------------
   -- Is_Auctionable --
   --------------------

   function Is_Auctionable
     (This : Transportation_Taxi)
      return Boolean
   is
      pragma Unreferenced (This);
   begin
      return True;
   end Is_Auctionable;

end Sancta.Ctree.Urus.Tasks.Compound;
