with Sancta.Component.Cost_Cache,
     Sancta.Component.Factory,
     Sancta.Component.Helper,
     Sancta.Component.Network,
     Sancta.Component.Types,
     Sancta.Criteria;

package body Sancta.Component.Bidder_Flat is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register
        (Name, Create'Access,
         (1 => Requires_Agent'Access,
          2 => Requires_Cost'Access,
          3 => Requires_Link'Access));
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Agpl.Xml.Node;
      Env    : Environment.Object)
      return Component.Object_Access
   is
      pragma Unreferenced (Env);
      Help : constant Helper.Object := Helper.Create (Config);
      This : constant Object_Access :=
               new Object
                 (Name'Access,
                  Config,
                  Component.Network.Network (Help.Input (Requires_Link)).Link,
                  Cost_Cache.Cost_Cache     (Help.Input (Requires_Cost)).Ptr,
                  Types.Agent               (Help.Input (Requires_Agent)).Agent);
   begin
      This.Verify (Option_Criterion);
      This.Verify (Option_Channel);
      This.Bdr.Create (Chan           =>
                         Sancta.Network.Value (This.Option (Option_Channel, "")),
                       Crit           =>
                         Criteria.Value (This.Option (Option_Criterion, "")));

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      This.Bdr.Run;

      Next := Clock + 0.01;
   end Run;

end Sancta.Component.Bidder_Flat;
