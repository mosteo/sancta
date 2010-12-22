with Agpl.Drawing; use Agpl.Drawing;
with Agpl.Trace; use Agpl.Trace;
with Agpl.Unique_Id; use Agpl.Unique_Id;
use Agpl;

with Sancta.Component; use Sancta.Component;
with Sancta.Component.Types; use Sancta.Component.Types;

procedure T006_Interfaces is
   M  : Map_Data;
   Mm : constant Unique_Data :=
          (Id   => Value ("test"),
           Data => Data_Handles.Set (M));
   X  : Paths;
   Xx : constant Unique_Data :=
          (Id   => Value ("test"),
           Data => Data_Handles.Set (X));
begin
   Log ("M  in Data: " & Boolean'Image (M  in Data'Class), Always);
   Log ("MM in Data: " & Boolean'Image (Mm in Data'Class), Always);
   Log ("M  in Draw: " & Boolean'Image (M  in Drawable'Class), Always);
   Log ("MM in Draw: " & Boolean'Image (Mm in Drawable'Class), Always);
   Log ("MM in Uniq: " & Boolean'Image (Mm in Univocally_Identifiable'Class), Always);
   Log ("M? in Draw: " & Boolean'Image
        (Mm.Data.Ref.all in Drawable'Class), Always);
   Log ("XX in Draw: " & Boolean'Image (XX in Drawable'Class), Always);
   Log ("X? in Draw: " & Boolean'Image
        (Xx.Data.Ref.all in Drawable'Class), Always);
exception
   when E : others =>
      Log (Report (E), Always);
end T006_Interfaces;
