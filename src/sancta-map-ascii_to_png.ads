--  Procedure which converts an ASCII file to a bitmap PNG.

with Ada.Strings.Maps; use Ada.Strings.Maps;

-----------------------------
-- Sancta.Map.Ascii_To_Png --
-----------------------------
--  Everything that is not obstacle, it is free.
procedure Sancta.Map.Ascii_To_Png (In_Name  : String;
                                   Out_Name : String;
                                   Obstacle : Character_Set);
