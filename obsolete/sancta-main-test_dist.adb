with Sancta.Config;
with Sancta.Distributed;
with Sancta.Distributed.Datastore;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace;        use Agpl.Trace;
with Agpl; use Agpl;

with Gnat.Os_Lib;

with Text_Io; use Text_Io;

procedure Sancta.Main.Test_Dist is
   type Int is new Distributed.Object_Data with record
      I : Integer;
   end record;
begin
   declare
      Our_Id : aliased Node_Id;
      Link   : aliased Network.Layer.Udp.Object (Our_Id'Access);
      Db     :         Distributed.Datastore.Object (Link'Access);

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown is
      begin
         Gnat.Os_Lib.Os_Exit (-1);
      end Shutdown;

      -----------
      -- Usage --
      -----------

      procedure Usage is
      begin
         Put_Line ("Usage:");
         Put_Line (Program_Name & " --id <node_id> --config <file>");
      end Usage;

   begin
      Log ("Starting...", Always);

      if not Exists ("--id") or else not Exists ("--config") then
         Usage;
         Shutdown;
         return;
      end if;

      --  Common configuration.
      Config.Init (Link => Link'Unrestricted_Access, Log_Level => Debug);

      Our_Id := Config.Node_Id;

      --  Set up comms.
      Network.Groups.Init (Config.Options);
      Link.Init (Config.Options);

      Log ("Ready.", Informative);

      loop
         delay 1.0;
      end loop;

   end;

exception
   when E : others =>
      Put_Line ("[Main]: " & Report (E));
      Log ("[Main]: " & Report (E), Error);
--      Shutdown;
end Sancta.Main.Test_Dist;
