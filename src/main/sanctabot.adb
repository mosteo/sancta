with Sancta.Starter;

with Agpl.Trace; use Agpl.Trace;

with Ada.Text_Io; use Ada.Text_Io;

procedure Sanctabot is
begin
   Sancta.Starter.Launch;
exception
   when E : others =>
      Put_Line ("Sanctabot [Main]: " & Report (E));
      Log      ("Sanctabot [Main]: " & Report (E), Error);
      --      Shutdown;
end Sanctabot;

