with Ada.Containers.Ordered_Maps,
     Ada.Containers.Vectors;

generic
   type Vertex_Id   is private;
   type Vertex_Data is private;
   with function "<"   (L, R : Vertex_Id)   return Boolean is <>;
   with function Image (Id   : Vertex_Id)   return String is <>;
   with function Image (Data : Vertex_Data) return String is <>;
package Sancta.Map.Graph is

   --   An arbitrary topological map.
   --   Stored as adjacency lists for each vertex.

   Log_Section : constant String := "sancta.map.graph";

   type Object is abstract new Map.Object with private;

   type Location is abstract new Map.Location with private;

   overriding
   function "=" (L, R : Location) return Boolean;

   overriding
   function "<" (L, R : Location) return Boolean;

   overriding
   function Image (This : Location) return String;
   --  Returns id_image plus data image

   overriding
   function Num_Neighbors (L : Location) return Natural;

   overriding
   function Neighbor (L : Location; I : Positive) return Location;

   overriding
   function Is_Neighbor (L, R : Location) return Boolean;

   overriding
   function Real_Cost (Ini, Fin : Location) return Costs;
   --  Real cost of traversing from Ini to Fin

   type Observation is new Map.Observation with record
      Data : Vertex_Data;
   end record;

   -----------------------------------------------------------
   --  Particular functions, for creation of this kind of map:
   -----------------------------------------------------------

   procedure Add_Vertex (This : in out Object;
                         Id   :        Vertex_Id;
                         Data :        Vertex_Data);

   procedure Add_Edge (This : in out Object;
                       V1,
                       V2   :        Vertex_Id;
                       Cost :        Costs);


private

   type Location is abstract new Map.Location with record
      M  : access Object;
      Id : Vertex_Id;
   end record;

   type Neighbor_Type is record
      Loc  : Location;
      Cost : Costs;
   end record;

   package Vertex_Vectors is new Ada.Containers.Vectors
     (Positive,
      Neighbor_Type);

   package Vertex_Edges is new Ada.Containers.Ordered_Maps
     (Location,
      Vertex_Vectors.Vector,
      "<",
      Vertex_Vectors."=");

   --  Possible speed-up: instead of storing vectors, store a pointer to vector
   --  so accesses are by reference instead of by copy.

   type Object is new Map.Object with record
      Bself : access Object := Object'Unchecked_Access;
      Edges : Vertex_Edges.Map;
   end record;

end Sancta.Map.Graph;
