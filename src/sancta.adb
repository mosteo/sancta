with Agpl.Strings;

package body Sancta is

--     function "=" (L, R : in Node_Id) return Boolean is
--        use Ids;
--     begin
--        return Ids.Bounded_String (L) = Ids.Bounded_String (R);
--     end "=";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : in Node_Id) return Boolean is
      use Ids;
   begin
      return Ids.Bounded_String (L) < Ids.Bounded_String (R);
   end "<";

   -----------
   -- Image --
   -----------

   function Image (Id : in Node_Id) return String is
   begin
      if Allow_Uppercase_Node_Ids then
         return To_String (Id);
      else
         return Agpl.Strings.L (To_String (Id));
      end if;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Id : in String) return Node_Id is
   begin
      if Id'Length = 0 then
         raise Constraint_Error with "Invalid Node_Id";
      end if;
      if Allow_Uppercase_Node_Ids then
         return To_Bounded_String (Id);
      else
         return To_Bounded_String (Agpl.Strings.L (Id));
      end if;
   end Value;

   -------------
   -- No_Node --
   -------------

   function No_Node return Node_Id is
   begin
      return To_Bounded_String ("no node");
   end No_Node;

   ---------------
   -- All_Nodes --
   ---------------

   function All_Nodes return Node_Id is
   begin
      return To_Bounded_String ("all nodes");
   end All_Nodes;

   -----------
   -- Value --
   -----------

   function Value (Id_1, Id_2 : String) return Unordered_Node_Pair is
   begin
      if Id_1 < Id_2 then
         return (Value (Id_1), Value (Id_2));
      else
         return (Value (Id_2), Value (Id_1));
      end if;
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Id_1, Id_2 : Node_Id) return Unordered_Node_Pair is
   begin
      return Value (Image (Id_1), Image (Id_2));
   end Value;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Unordered_Node_Pair) return Boolean is
   begin
      return L.L < R.L or else (L.L = R.L and then L.R < R.R);
   end "<";

end Sancta;
