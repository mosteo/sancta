--  Procedure which converts an ASCII file to a bitmap PNG.

with Sancta.Map.Ascii_To_Png;

with Agpl.Strings;     use Agpl.Strings;
with Agpl.Trace;       use Agpl.Trace;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_Io;      use Ada.Text_Io;

------------------
-- Ascii_To_Png --
------------------

procedure Ascii_To_Png is
   procedure Usage is
   begin
      Put_Line ("Usage:");
      Put_Line ("   " & Command_Name & " <in_file.txt> <out_file.png> <obstacle chars>");
   end Usage;
begin
   if Argument_Count /= 3 or else
     not Contains (Argument (2), "png")
   then
      Usage;
   else
      Sancta.Map.Ascii_To_Png (Argument (1),
                               Argument (2),
                               To_Set (Argument (3)));
   end if;
exception
   when E : others =>
      Put_Line ("Exception: " & Report (E));
      Usage;
end Ascii_To_Png;
