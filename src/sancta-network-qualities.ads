with Agpl.Containers.Indefinite_Matrix2d;
with Agpl.Containers.String_Vectors;

package Sancta.Network.Qualities is

   pragma Preelaborate;

   type Quality is delta 0.01 range 0.0 .. 100.0;
   --  100.0 is perfect quality.

   type Map (Directed_Source   : Boolean;
             Directed_Query    : Boolean;
             Missing_As_Broken : Boolean)
     is tagged private;
   --  Directed source: the links have directionality in its quality.
   --  Directed query : if false, the worst quality is reported
   --    (Makes no difference in undirected sources).
   --  Missing as broken: a missing quality is reported as 0.0, instead of exception.

   --  We don't want derived types; at most subtypes with specialized discriminants.
   --  That's why we use classwide subprograms below

   function Get (This : Map'Class; From, To : Node_Id) return Quality;
   --  If Directed_Query, this is the directed quality. Otherwise, lowest of the two.

   procedure Set (This : in out Map'Class; From, To : Node_Id; Q : Quality);
   --  If directed source, only half-duplex info is stored.
   --  Otherwise, both directions are filled-in.

   package Node_Vectors renames Agpl.Containers.String_Vectors;
   subtype Node_Vector is Node_Vectors.Vector;

   function Nodes (This : Map'Class) return Node_Vector;

private

   package Qmatrix is new Agpl.Containers.Indefinite_Matrix2d
     (Node_Id, Quality);

   type Map (Directed_Source   : Boolean;
             Directed_Query    : Boolean;
             Missing_As_Broken : Boolean)
     is tagged record
      Matrix : Qmatrix.Matrix;
   end record;

end Sancta.Network.Qualities;
