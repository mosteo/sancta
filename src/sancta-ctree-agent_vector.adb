package body Sancta.Ctree.Agent_Vector is

   ---------
   -- Add --
   ---------

   procedure Add
     (This : in out Object;
      Pose :        Types.Pose)
   is
   begin
      This.V.Append (Pose);
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Object) is
   begin
      This.V.Clear;
   end Clear;

   ------------
   -- Result --
   ------------

   function Result (This : Object) return Types.Pose is
      use type Types.Pose;
      Y : Types.Pose := Types.Origin;
   begin
      for I in This.V.First .. This.V.Last loop
         Y := Y + This.V.Vector (I);
      end loop;

      --  return Y / Types.Real (This.Length);
      return Y;
   end Result;

end Sancta.Ctree.Agent_Vector;
