package body Sancta.Network.Qualities is

   ---------
   -- Get --
   ---------

   function Get (This : Map'Class; From, To : Node_Id) return Quality is
      Q1, Q2 : Quality;
      Found1 : Boolean;
      Found2 : Boolean;
   begin
      if This.Directed_Query then
         This.Matrix.Get (From, To, Q1, Found1);
         This.Matrix.Get (To, From, Q2, Found2);
         if Found1 and then Found2 then
            return Quality'Min (Q1, Q2);
         elsif This.Missing_As_Broken then
            return 0.0;
         elsif (not Found1) and (not Found2) then
            raise Constraint_Error with
              "No Q (both ways) from " & Image (From) & " to " & Image (To);
         elsif not Found1 then
            raise Constraint_Error with
              "No Q (one way) from " & Image (From) & " to " & Image (To);
         elsif not Found2 then
            raise Constraint_Error with
              "No Q (one way) from " & Image (To) & " to " & Image (From);
         else
            raise Program_Error with "Should be unreachable";
         end if;
      else
         This.Matrix.Get (From, To, Q1, Found1);
         if Found1 then
            return Q1;
         elsif This.Missing_As_Broken then
            return 0.0;
         else
            raise Constraint_Error
              with "No Q found from " & Image (From) & " to " & Image (To);
         end if;
      end if;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Map'Class; From, To : Node_Id; Q : Quality) is
   begin
      This.Matrix.Set (From, To, Q);
      if not This.Directed_Source then
         This.Matrix.Set (To, From, Q);
      end if;
   end Set;

   -----------
   -- Nodes --
   -----------

   function Nodes (This : Map'Class) return Node_Vector is
      Rows   : constant Qmatrix.Index_Vector := This.Matrix.Row_Indexes;
      Result :          Node_Vector;
   begin
      for I in Rows.First_Index .. Rows.Last_Index loop
         Result.Append (Image (Rows.Element (I)));
      end loop;

      return Result;
   end Nodes;

end Sancta.Network.Qualities;
