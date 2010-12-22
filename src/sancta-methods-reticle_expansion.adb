with Sancta.Tasks.Panorama_At_Coords;
with Sancta.Tasks.Search_Blob_In;
with Sancta.Types.Real_Math;

with Ada.Numerics;

package body Sancta.Methods.Reticle_Expansion is

   package Math renames Sancta.Types.Real_Math;
   Pi : constant := Ada.Numerics.Pi;

   use type Types.Real;

   -----------
   -- Apply --
   -----------

   function Apply
     (This : in Object;
      That : in Sancta.Tasks.Object'Class)
      return Sancta.Plan_Node.Node_Access
   is
      use Sancta.Plan_Node.Node_Lists;
      Nodes : List;
   begin
      --  Empty result if not appropriate.
      if That not in Tasks.Search_Blob_In.Object'Class then
         return null;
      end if;

      declare
         L8 : constant Types.Real := 2.0 * This.Rang * Math.Tan (Pi / 8.0);
         --  Octogon segment size
         A8 : constant Types.Real := Pi * 6.0 / 8.0;
         --  Inner octogon angles

         A6 : constant Types.Real := Pi * 4.0 / 6.0;
         --  Inner hexagon angles

         L4 : constant Types.Real := 2.0 * L8 * Math.Sin (A8 / 2.0);
         --  Square segment length.
         L6 : constant Types.Real := L8 / (2.0 * Math.Sin (Pi / 8.0));
         --  Hexagon segment size.

         H6 : constant Types.Real := L6 * Math.Cos ((Pi - A6) / 2.0);
         --  Apothem of hexagon.

         Area : Tasks.Search_Blob_In.Object renames
           Tasks.Search_Blob_In.Object (That);

         Horiz_Step : Types.Real;
         Vert_Step  : Types.Real;
         Odd_Row    : Boolean := True;
         Pan        : Tasks.Panorama_At_Coords.Object;
         X          : Types.Real renames Pan.Coords.X;
         Y          : Types.Real renames Pan.Coords.Y;
         InitX      : Types.Real;
      begin
         case This.Kind is
            when Rect =>
               Horiz_Step := L4;
               Vert_Step  := L4;
               Y          := Area.Bottom + L4 / 2.0;
               X          := Area.Left   + L4 / 2.0;
            when Hex =>
               Horiz_Step := 3.0 * L6;
               Vert_Step  := 2.0 * H6;
               X          := Area.Left + L6 / 2.0;
               Y          := Area.Bottom + H6;
         end case;

         InitX := X;
         while Y < Area.Top loop
            while X < Area.Right loop
               Append (Nodes, Plan_Node.Create (Pan));
               Pan.Assign_Id;
               X := X + Horiz_Step;
               --  Ensure full exploration of the right margin:
               if X >= Area.Right then
                  X := X - Horiz_Step / 2.0;
               end if;
            end loop;
            Y := Y + Vert_Step;
            --  In the last step up, we must ensure we're not forgetting any
            --  terrain, so we'll check using a half step.
            if Y >= Area.Top then
               Y := Y - Vert_Step / 2.0;
            end if;

            Odd_Row := not Odd_Row;
            if This.Kind = Hex then
               if Odd_Row then
                  X := InitX;
               else
                  X := InitX + 1.5 * L6;
               end if;
            else
               X := InitX;
            end if;
         end loop;
      end;

      return Plan_Node.Create (Plan_Node.And_Node, Nodes);
   end Apply;

end Sancta.Methods.Reticle_Expansion;
