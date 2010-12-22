with Sancta.Component.Factory,
     Sancta.Component.Helper,
     Sancta.Component.Network;

package body Sancta.Component.Auctioneer is

   type Object_Access is access all Object'Class;

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
      return Component.Object_Access
   is
      This : constant Object_Access :=
               new Object
                 (Name'Access,
                  Config,
                  Component.Network.Network
                    (Helper.Create (Config).Input (Requires_Link)).Link);
   begin
      This.Subscribe (Requires_Item);
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
      Next := Clock + 0.01;
      This.Auc.Run;
   end Run;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
   begin
      This.Auc.Add_Item (Item (Value).Item.Ref.all);
   end Key_Stored;

end Sancta.Component.Auctioneer;
