with Sancta.Gui.Robot_Data;
with Sancta.Network;
with Sancta.Network.Layer;

with Agpl.Trace.Root;

package Sancta.Trace is

--   pragma Elaborate_Body;

   subtype Levels is Agpl.Trace.Levels;

   function Message (Text    : in String;
                     Level   : in Levels;
                     Section : in String) return Gui.Robot_Data.Network_Update;
   --  Create a trace message for the network layer.

   type Object (Link : access Network.Layer.Object'Class;
                Dest : access Network.Address) is
     new Agpl.Trace.Root.Object with null record;

   overriding
   procedure Log
     (This    : in out Object;
      Text    : in String;
      Level   : in Levels;
      Section : in String := "");
   --  Overriden to propagate a message via network.

end Sancta.Trace;
