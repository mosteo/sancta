with Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Located_Agent;

package body Sancta.Ctree.Component.Hack is

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
      return Sancta.Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
      Bah  :          Ada.Calendar.Time;
   begin
      This.Subscribe (Requires_Base_Agent);
      This.Run (Bah);
      return Sancta.Component.Object_Access (This);
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
      if (not This.Map_Passed) and then This.Exists (Requires_Map) then
         This.Map_Passed := True;
         Robot.The_Map := Types.Map_Data (This.Input (Requires_Map)).Map;
         Next := Clock + 1000.0;
         Log ("Passing map along", Debug, Log_Section);
      elsif This.Map_Passed then
         Next := Clock + 1000.0;
      else
         Next := Clock + 0.1;
      end if;
   end Run;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
      Ag  : constant Located_Agent.Object'Class :=
        Located_Agent.Object'Class (Sancta.Component.Types.Agent (Value).Agent.all);
   begin
      This.Bot.Set_Name (Ag.Get_Name);
      Robot.Object (This.Bot.all).Set_Pose (Ag.Get_Pose);
      This.Output (Provides_Base,
                   Sancta.Component.Types.Agent'(Data with This.Bot));
   end Key_Stored;


end Sancta.Ctree.Component.Hack;
