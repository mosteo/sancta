with Sancta.Types;

with Agpl.Xml;

--  Utility functions for querying the loaded options.

package Sancta.Config.Query is

   --  pragma Elaborate_Body;

   function Agent_Kind (Agent : in String) return Types.Agent_Kinds;
   --  Check for agent kinds

end Sancta.Config.Query;
