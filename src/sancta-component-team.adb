with Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Component.Utils;

with Agpl.Strings;

--  with Sancta.Agent_Proxy;

package body Sancta.Component.Team is

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
     (Config : Agpl.Xml.Node)
      return Component.Object_Access
   is
      use Agpl.Strings,
          Utils;
      This : constant Object_Access := new Object (Name'Access, Config);
      Num  : constant Natural       := Option (This.all, Option_Amount, 0);

   begin
      for I in 1 .. Num loop
         This.Add_Correspondence
           (Requires_Agent & Internal_Key (Trim (I'Img)),
            This.Ekey (Requires_Agent) & External_Key (Trim (I'Img)));
         This.Subscribe (Requires_Agent & Internal_Key (Trim (I'Img)));
         if This.Exists (Requires_Agent & Internal_Key (Trim (I'Img))) then
            This.Team.Set_Agent
              (Types.Agent
                 (This.Input
                    (Requires_Agent & Internal_Key (Trim (I'Img)))).Agent.all);
         end if;
      end loop;

      if This.Exists (Option_Period) then
         This.Period.Set_Period (Duration'Value (This.Option (Option_Period)));
      end if;

      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
      --  The received value is an access to agent, that we set.
   begin
      This.Team.Set_Agent (Types.Agent (Value).Agent.all);

--      Agent_Proxy.Object (Types.Agent (Value).Agent.all).Print;
   end Key_Stored;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar,
          Utils;
      Num  : constant Natural := Option (This, Option_Amount, 0);
   begin
      This.Period.Next (Next);
      if Natural (This.Team.Get_Agents.Length) = Num then
         This.Output (Provides_Team, Types.Teams'(Data with This.Team));
      end if;
   end Run;

end Sancta.Component.Team;
