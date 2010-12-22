with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Xml;
with Agpl; use Agpl;

with Gnat.Os_Lib;

with Sancta.Config;
with Text_Io; use Text_Io;

pragma Warnings (Off);
with Agpl.Task_Termination;
with Sancta.Tasks.Include;
pragma Warnings (On);

package body Sancta.Starter is

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      delay 0.1;
      Gnat.Os_Lib.Os_Exit (0);
   exception
      when E : others =>
         Log ("Shutting down: " & Report (E), Warning, Log_Section);
         Gnat.Os_Lib.Os_Exit (-1);
   end Shutdown;

   --------------------
   -- Shutdown_Abort --
   --------------------

   procedure Shutdown_Abort (Code : Integer) is
   begin
      delay 1.0;
      Gnat.Os_Lib.Os_Exit (Code);
   exception
      when E : others =>
         Log ("Sancta.Starter: Aborting: " & Report (E), Warning, Log_Section);
         Gnat.Os_Lib.Os_Exit (-1);
   end Shutdown_Abort;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("Usage:");
      Put_Line (Program_Name & " --id <node_id> --config <file> [OPTIONS]");
      Put_Line ("   No options defined for now.");
   end Usage;

   ------------
   -- Launch --
   ------------

   procedure Launch is
   begin
      if not Exists ("--id") or else not Exists ("--config") then
         Usage;
         Shutdown;
         return;
      end if;

      --  Common configuration.
      Log ("Starting...", Always);
      Config.Init (Value (Get_Option ("--id")), get_option ("--config"));
      Log ("Configuration parsed:", Always);
      Log (Xml.To_String (Config.Get_All_Options), Always);

      Config.Create_Plugins;

      Config.Set_Log_Options;
      Config.Enable_Log_Sections;
      --  Albeit already done within plugin creation, necessary again in case
      --  some logger has been created and added by some plug-in, after the
      --  first call...

      Log ("SANCTA node " & Image (Config.Get_Id) & " ready", Informative);

      --  We are done, all in plugins hands...
   end Launch;

   ------------
   -- Launch --
   ------------

   procedure Launch (Id         :     Node_Id;
                     Config_Xml :     String)
   is
   begin
      --  Common configuration.
      Log ("Starting...", Always);
      Config.Init_Str (Id, Config_Xml);
      Log ("Configuration parsed:", Always);
      Log (Xml.To_String (Config.Get_All_Options), Always);

      Config.Create_Plugins;

      Config.Set_Log_Options;
      Config.Enable_Log_Sections;
      --  Albeit already done within plugin creation, necessary again in case
      --  some logger has been created and added by some plug-in, after the
      --  first call...

      Log ("SANCTA node " & Image (Config.Get_Id) & " ready", Informative);

      --  We are done, all in plugins hands...
   end Launch;

end Sancta.Starter;
