with Agpl.Conversions; use Agpl.Conversions;
with Agpl.Strings.Fields; use Agpl.Strings.Fields;
with Agpl.Ustrings; use Agpl.Ustrings;

package body Sancta.Ctree.Message is

   use type Data_Server.Goals;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Object) return String is
      Result : Ustring;
      use Asu;
   begin
      Asu.Append (Result, This.Kind'Img);
      Asu.Append (Result, " ");

      case This.Kind is
         when Use_Goals =>
            Asu.Append (Result, To_String (This.Goals'Length) & " ");
            for I in This.Goals'Range loop
               declare
                  G : constant Data_Server.Goal_Ada := This.Goals (I);
               begin
                  Asu.Append (Result, To_String (G.X) & " ");
                  Asu.Append (Result, To_String (G.Y) & " ");
                  Asu.Append (Result, G.Active'Img & " ");
               end;
            end loop;
         when Goal_Reached | Shutdown =>
            Asu.Append (Result, "0 "); -- To avoid exception in the other side.
      end case;

      pragma Assert (Length (Result) < 1000);

      return +Result;
   end To_String;

   -----------
   -- Parse --
   -----------

   function Parse (S    : String) return Object is
      This : Object (Kinds'Value (Select_Field (S, 1)),
                     Positive'Value (Select_Field (S, 2)));
   begin
      case This.Kind is
         when Use_Goals =>
            declare
               Fi : Positive := 3;
            begin
               for Gi in This.Goals'Range loop
                  This.Goals (Gi).X := Float'Value (Select_Field (S, Fi));
                  This.Goals (Gi).Y := Float'Value (Select_Field (S, Fi + 1));
                  This.Goals (Gi).Active := Boolean'Value (Select_Field (S, Fi + 2));
                  Fi := Fi + 3;
               end loop;
            end;
         when Goal_Reached | Shutdown =>
            null;
      end case;
      return This;
   end Parse;

end Sancta.Ctree.Message;
