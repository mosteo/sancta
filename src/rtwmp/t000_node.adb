with Agpl.Trace;
with Sancta.Component.Include;
with Sancta.Component.Network_Rtwmp;
with Sancta.Node;

--  Launches a node with all the sancta components registered.

procedure T000_Node is

   use Agpl.Trace;

   type Object is new Sancta.Node.Object with null record;

   overriding
   procedure Register_Components (This : in out Object);

   -------------------------
   -- Register_Components --
   -------------------------

   procedure Register_Components (This : in out Object) is
      pragma Unreferenced (This);
   begin
      Sancta.Component.Include.Register;
      Sancta.Component.Network_Rtwmp.Register;
   end Register_Components;

   This : Object;
   pragma Unreferenced (This);

begin
   null;
exception
   when E : others =>
      Log ("Main: " & Report (E), Error);
end T000_Node;
