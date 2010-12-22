with Sancta.Debug;
with Sancta.Types;
with Sancta.Types.Operations;

with Text_Io; use Text_Io;

procedure T001_Segment_Ratio is
   use Sancta.Debug;
   use Sancta.Types; use Operations;
   P1 : constant Pose := (1.0, 1.0, 0.0);
   P2 : constant Pose := (5.0, 5.0, 1.0);
begin
   Put_Line ("P1 = " & To_String (P1));
   Put_Line ("P2 = " & To_String (P1));
   Put_Line ("0.5 = " & To_String (Segment_Ratio (P1, P2, 0.5)));
   Put_Line ("0.1 = " & To_String (Segment_Ratio (P1, P2, 0.1)));
end T001_Segment_Ratio;
