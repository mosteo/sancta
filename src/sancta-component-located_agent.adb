with Agpl.Trace;
with Sancta.Agent,
     Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Debug2;

--  use Agpl.Trace;

package body Sancta.Component.Located_Agent is

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
      This : constant Object_Access := new Object (Name'Access,
                                                   Config);
   begin
      This.Agent := new Agent_Proxy.Object;

      if This.Option (Option_Name, "") /= "" then
         This.Agent.Set_Name (This.Option (Option_Name, ""));
      else
         raise Constraint_Error with "Name for agent is required";
      end if;

      if This.Exists (Requires_Pose) then
         This.Agent.Set_Pose (Types.Pose (This.Input (Requires_Pose)).Pose);
         This.Output (Provides_Agent,
                      Types.Agent'(Data with Agent.Object_Access (This.Agent)));
         This.Given := True;
      end if;

      This.Subscribe (Requires_Pose);
      This.Subscribe (Requires_Velo);

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
   begin
      if Key = Requires_Pose then
         This.Agent.Set_Pose (Types.Pose (Value).Pose);
         Log ("Pose: " & Debug2.To_String (This.Agent.Get_Pose),
              Debug, Log_Section);
      elsif Key = Requires_Velo then
         This.Agent.Set_Velo (Types.Pose (Value).Pose);
         Log ("Velo: " & Debug2.To_String (This.Agent.Get_Velo),
              Debug, Log_Section);
      end if;
      This.Output (Provides_Agent,
                   Types.Agent'(Data with Agent.Object_Access (This.Agent)));
      This.Given := True;
   end Key_Stored;

end Sancta.Component.Located_Agent;
