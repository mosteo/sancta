package body Sancta.Map.Graph is

   ---------
   -- "=" --
   ---------

   function "="
     (L, R : Location)
      return Boolean
   is
   begin
      return L.Id = R.Id;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<"
     (L, R : Location)
      return Boolean
   is
   begin
      return L.Id < R.Id;
   end "<";

   -----------
   -- Image --
   -----------

   function Image
     (This : Location)
      return String
   is
   begin
      return
        Image (This.Id) & ": " &
        Image (Observation (This.M.Get_At (This)).Data);
   end Image;

   -------------------
   -- Num_Neighbors --
   -------------------

   function Num_Neighbors
     (L : Location)
      return Natural
   is
   begin
      return Natural (L.M.Edges.Element (L).Length);
   end Num_Neighbors;

   --------------
   -- Neighbor --
   --------------

   function Neighbor
     (L : Location;
      I : Positive)
      return Location
   is
   begin
      return L.M.Edges.Element (L).Element (I).Loc;
   end Neighbor;

   -----------------
   -- Is_Neighbor --
   -----------------

   function Is_Neighbor
     (L, R : Location)
      return Boolean
   is
   begin
      return L.M.Edges.Element (L).Contains (R);
   end Is_Neighbor;

   ---------------
   -- Real_Cost --
   ---------------

   function Real_Cost
     (Ini, Fin : Location)
      return Costs
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return Real_Cost (Ini, Fin);
   end Real_Cost;

   -------------------
   -- Estimate_Cost --
   -------------------

   function Estimate_Cost
     (Ini, Fin : Location)
      return Costs
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return Estimate_Cost (Ini, Fin);
   end Estimate_Cost;

   --------------
   -- Position --
   --------------

   function Position
     (Here, There : Location)
      return Map.Relative_Position'Class
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return Position (Here, There);
   end Position;

   ----------------
   -- Add_Vertex --
   ----------------

   procedure Add_Vertex
     (This : in out Object;
      Id   :        Vertex_Id;
      Data :        Vertex_Data)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
   end Add_Vertex;

   --------------
   -- Add_Edge --
   --------------

   procedure Add_Edge
     (This : in out Object;
      V1,
                       V2   :        Vertex_Id;
      Cost :        Costs)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
   end Add_Edge;

end Sancta.Map.Graph;
