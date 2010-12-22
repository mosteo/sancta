with Sancta.Starter;

package body Sancta.Node is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out Object)
   is
   begin
      Object'Class (This).Register_Components;
      Starter.Launch;
   end Initialize;

end Sancta.Node;
