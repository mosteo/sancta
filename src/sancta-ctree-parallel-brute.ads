with Ada.Containers.Ordered_Multisets;
with Ada.Finalization;
with Agpl.Containers.Integer_Sets;
with Agpl.Containers.Naked_Vectors;

package Sancta.Ctree.Parallel.Brute is

   Log_Section : constant String := "Sancta.Ctree.parallel.brute";

   Do_Report : constant Boolean := False;
   Detailed  : constant Boolean := False;

   Use_History : constant Boolean := False;

   function Evaluate (Map       :     Sancta.Map.Object'Class;
                      Navigator :     Tree_Navigator.Object'Class;
                      Tree      :     Path_Trees.Tree;
                      --  Nodes of Parallel.Node_Data
                      Bots      :     Positive;
                      Limit     :     Network_Range;
                      Criterion :     Sancta.Criteria.Enum_Criteria;
                      Stop      :     Sancta.Costs) return Sancta.Costs;
   --  May raise Cost_Exceeded

private

   package Cursor_Vectors is new
     Agpl.Containers.Naked_Vectors (Path_Trees.Cursor);

   Context_Extinguished : exception;
   --  For when this context branches into others and must be eliminated

   type Directions is (Up, Down, Wait);
   --  Wait is for teams stupidly waiting at a split for other to merge with'em

   type Teams is tagged record
      Local_Time : Sancta.Costs;
      Bots       : Positive;
      Direction  : Directions;
      Pos        : Path_Trees.Cursor;
      Id         : Natural;
   end record;

   type Team_Array is array (Positive range <>) of Teams;

   function "<" (L, R : Teams) return Boolean; pragma Inline ("<");

   function Node (This : Teams) return access Node_Data;

   procedure New_Id (This : in out Teams);

   package Team_Sets is new Ada.Containers.Ordered_Multisets (Teams);

   type Recursive_Context is new Ada.Finalization.Controlled with record
      Id       : Natural;

      Pending    : Tc.Lists.List;
      Tasks_Done : Natural := 0;

      Teams    : Team_Sets.Set;

      Sum      : Sancta.Costs;
      Ave      : Sancta.Costs;

      History  : Agpl.Containers.Integer_Sets.Set;
   end record;

   procedure New_Id (This : in out Recursive_Context);

   function Latest_Time (This : Recursive_Context) return Sancta.Costs;
   pragma Inline (Latest_Time);

   function Earliest_Time (This : Recursive_Context) return Sancta.Costs;
   pragma Inline (Earliest_Time);

   function "<" (L, R : Recursive_Context) return Boolean; pragma Inline ("<");

   function Current_Cost (This      : Recursive_Context;
                          Criterion : Sancta.Criteria.Enum_Criteria)
                          return      Sancta.Costs;

   function Is_Done (This : Recursive_Context) return Boolean;

   function Someone_Here_Or_Below (This : Recursive_Context;
                                   Pos  : Path_Trees.Cursor)
                                   return Boolean;

   function Someone_Strictly_Below (This : Recursive_Context;
                                    Pos  : Path_Trees.Cursor)
                                    return Boolean;

   package Context_Sets is new Ada.Containers.Ordered_Multisets
     (Recursive_Context);

   subtype Bool_Array is Boolean_Array;

end Sancta.Ctree.Parallel.Brute;
