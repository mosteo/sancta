--  Utility functions for querying the loaded options.

package body Sancta.Config.Query is

   ----------------
   -- Agent_Kind --
   ----------------

   function Agent_Kind (Agent : in String) return Types.Agent_Kinds is
      use Agpl.Xml;

      Agent_Node : constant Xml.Node := Agent_Options (Agent);
   begin
      if Agent_Node = null then
         return Types.Unknown;
      end if;

      begin
         return Types.Agent_Kinds'Value (Get_Attribute (Agent_Node,
                                                        "kind",
                                                        Default => "Unknown"));
      exception
         when Constraint_Error => -- Kind not properly added here
            return Types.Unknown;
      end;
   end Agent_Kind;

   -------------------
   -- Agent_Options --
   -------------------

   function Agent_Options (Agent : in String) return Xml.Node is
      use Agpl.Xml;
      Agents : constant Node_Array := Get_All (Options, "agent");
   begin
      for I in Agents'Range loop
         if Get_Attribute (Agents (I), "id") = Agent then
            return Agents (I);
         end if;
      end loop;

      return null;
   end Agent_Options;

end Sancta.Config.Query;
