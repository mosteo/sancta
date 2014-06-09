--  Bitmap map. Space efficient, only a bit of info per cell...

with Ada.Streams;

package Sancta.Map.Bitmap is

   --  pragma Preelaborate;

   Log_Section : constant String := "sancta.map.bitmap";

   pragma Bug_Workaround;
   Buggy_Stage_Png : constant Boolean := True;
   --  Bug: The last PNG line is not displayed when used as a map.
   --  So, when this flag is true, the PNG map loading function will
   --  discard the last PNG line

   type Vicinities is (Vicinity_4, Vicinity_6, Vicinity_8);
   --  Vicinity_6 is a mix of 4 and 8, were corners are navigated around with
   --  4-vicinity, but open spaces are 8-vicinity

   type Object (Vicinity : Vicinities) is
     new Map.Object with private;
   --  The map itself.

   not overriding
   procedure Set_Size (This : in out Object;
                       Xmin :        Abscissa;
                       Ymin :        Ordinate;
                       Xmax :        Abscissa;
                       Ymax :        Ordinate);

   not overriding
   function Get_X_Min (This : Object) return Abscissa;

   not overriding
   function Get_X_Max (This : Object) return Abscissa;

   not overriding
   function Get_Y_Min (This : Object) return Ordinate;

   not overriding
   function Get_Y_Max (This : Object) return Ordinate;

   not overriding
   procedure Set_Cell_Size (This : in out Object; Cell_Size : Float);
   --  Inform the equivalence cell/real, for further purposes

   type Object_Access is access all Object'Class;

   type Terrains is (Free, Obstacle);

   type Bit_Location is new Location with record
      M : Object_Access;
      X : Abscissa;
      Y : Ordinate;
   end record;

   overriding
   function Get_Map (L : Bit_Location) return access Map.Object'Class;

   type Directions is (S, Se, E, Ne, N, No, O, So);

   Directions_Img : constant array (Directions) of Character :=
                      ('v', '\', '>', '/', '^', '`', '<', ',');

   type Bit_Rel_Pos (Dir : Directions) is
     new Relative_Position with null record;

   overriding
   function "<" (L, R : Bit_Rel_Pos) return Boolean;

   overriding
   function Image (This : Bit_Rel_Pos) return String;

   overriding
   function "=" (X, Y : Bit_Location) return Boolean;

   overriding
   function "<" (X, Y : Bit_Location) return Boolean;

   overriding
   function Image (This : Bit_Location) return String;

   overriding
   function Num_Neighbors (L : Bit_Location)
                           return Natural;

   overriding
   function Neighbor (L : Bit_Location; I : Positive)
                      return Bit_Location;

   overriding
   function Real_Cost (Ini, Fin : Bit_Location) return Costs;
   --  Real cost of traversing from Ini to Fin

   overriding
   function Estimate_Cost (Ini, Fin : Bit_Location) return Costs;
   --  An optimistic estimation of cost between ini and fin
   --  In practice, euclidean distance

   overriding
   function Is_Neighbor (L, R : Bit_Location) return Boolean;
   --  A cell is not neighbor of itself!

   overriding
   function Position (Here, There : Bit_Location)
                      return        Relative_Position'Class;

   not overriding
   function Loc (This : Object;
                 X    : Abscissa;
                 Y    : Ordinate) return Bit_Location'Class;

   type Bit_Observation is new Observation with record
      Bit : Terrains;
   end record;

   function Is_Traversable (This : Bit_Observation) return Boolean;
   pragma Inline (Is_Traversable);

   type Matrix is array (Ordinate range <>, Abscissa range <>) of Terrains;
   --  pragma Pack (Matrix);
   --  Matrix is indexed as (Row, Col) i.e. (Y, X)

   procedure Copy_But_Costs (This :     Object;
                             Copy : out Object);
   --  Get a copy of this, but the costs remain uncopied (could be very costly).

   not overriding
   function Get_Cell_Size (This : Object) return Float;

   overriding
   function Is_Known (This : Object;
                      Loc  : Location'Class) return Boolean;
   --  Always true

   not overriding
   function Is_Known (This : Object;
                      X    : Abscissa;
                      Y    : Ordinate) return Boolean;
   --  Always true

   not overriding
   function Within_Bounds (This : Object;
                           X    : Abscissa;
                           Y    : Ordinate) return Boolean;
   --  Within map bounds?

   overriding
   function Get_At (This : Object;
                    Loc  : Location'Class) return Observation'Class;

   not overriding
   function Get_At (This : Object;
                    X    : Abscissa;
                    Y    : Ordinate) return Terrains;

   overriding
   procedure Set_At (This : in out Object;
                     Loc  : in     Location'Class;
                     Obs  : in     Observation'Class);

   not overriding
   procedure Set_At (This : in out Object;
                     X    :        Abscissa;
                     Y    :        Ordinate;
                     Bit  :        Terrains);

   not overriding
   procedure Fill (This : in out Object;
                   Xmin :        Abscissa;
                   Ymin :        Ordinate;
                   Xmax :        Abscissa;
                   Ymax :        Ordinate;
                   Bit  :        Terrains);
   --  Performs clipping.

   not overriding
   procedure Set (This : in out Object;
                  Full :        Matrix);
   --  Calls create, no need to do it in advance...

   not overriding
   function Get_Data (This : Object) return access constant Matrix;
   --  Directly peek at the bits!

   not overriding
   function Get_Closest_Free_To (This : Object;
                                 Loc  : Bit_Location'Class;
                                 Near : Neighbor_Function)
                                 return Bit_Location'Class;
   --  Get Loc if free or some free "closest".

   overriding
   procedure From_Png (This : in out Object;
                       File :        String;
                       Load :        Loader);
   --  Reverses the Y axis so it matches the stage simulator

   Stage_Loader : constant Loader;
   --  Use this loader for simulations in Stage. It places the grid at 0, 0
   --  and reverses Y axis. For use with Sancta.Tasks.Goto_Pose_Bitmap_Wavefront

   overriding
   procedure Print (This : Object);

   not overriding
   procedure Print (This : Object; Route : Location_Lists.List);

   --  Convenience functions for Wavefront

   function Vicinity_8 (M : Map.Object'Class;
                        L : Location'Class) return Location_Vectors.Vector;
   function Vicinity_6 (M : Map.Object'Class;
                        L : Location'Class) return Location_Vectors.Vector;
   function Vicinity_4 (M : Map.Object'Class;
                        L : Location'Class) return Location_Vectors.Vector;
   --  Of course these require M and L to be of the types in Bitmap
   --  They return only free cells
   --  To avoid problems with corners, 6-vicinity doesn't consider neighbors
   --    diagonally linked cells were there's a corner in between:
   --  XX 1XX
   --   2 3XX
   --  XXXXXX Here, 2-3 and 1-3 are, but 2-1 aren't.

   function Any_At_4 (M : Map.Object'Class;
                      L : Location'Class) return Location_Vectors.Vector;
   --  Returns any neighbor cell regardless of freeness

   function Get_Cost (M : Map.Object'Class;
                      O : Observation'Class) return Costs;
   --  1 for Free, Inf for Obstacle

   not overriding
   procedure Compute_Costs
     (Map      : in out Object;
      Get_Near : access function (M : Sancta.Map.Object'Class;
                                  L : Location'Class)
                                  return Location_Vectors.Vector;
      Get_Cost : access function (M : Sancta.Map.Object'Class;
                                  O : Observation'Class) return Costs);
   --  Create the cache of costs. Beware this creates a N*N matrix
   --   where N is the number of locations in the map (X*Y)

   overriding
   procedure Compute_Costs (This : in out Object);
   --  Use default inherited Best_Path between all locations

   not overriding
   function Get_Cost_Between
     (This : Object;
      X1    : Abscissa;
      Y1    : Ordinate;
      X2    : Abscissa;
      Y2    : Ordinate) return Costs;
   --  will raise something if Compute_Costs hasn't been called...

   not overriding
   function To_Grid (This  : Object;
                     Pose  : Types.Pose) return Map.Bitmap.Bit_Location'Class;

   overriding
   function Nearest_Location (This : Object;
                              Pose : Types.Pose) return Location'Class;

   overriding
   function Nearest_Pose (This : Object;
                          Loc  : Location'Class) return Types.Pose;

   function To_Pose (Ratio : Float;
                     Loc   : Map.Bitmap.Bit_Location) return Types.Pose;

   function To_Pose (This  : Object;
                     Loc   : Map.Bitmap.Bit_Location'Class) return Types.Pose;

   function Get_Distance (This : Object;
                          Ini,
                          Fin  : Location'Class) return Types.Real;

   function Get_Distancing (This : Object;
                            Path : Location_Vectors.Vector) return Types.Real;
   --  Computes the necessary *maximum* distance between two robots,
   --   one at each end initially of the path, if they must close across this
   --   path.

   function Get_Min_Distance (This : Object;
                              Pose : Types.Pose;
                              Path : Map.Path) return Types.Real;
   --  Return the minimum distance along path, to pose
   --  Doesn't consider obstacles, but euclidean distance

   function Get_Min_Cost (This : Object;
                          Loc  : Bit_Location'Class;
                          Path : Map.Path) return Costs;
   --  Minimum path cost from a location to the closest one in path

   function Get_Min_Cost_Cell (This : Object;
                               Loc  : Bit_Location'Class;
                               Path : Map.Path) return Bit_Location'Class;
   --  Get the cell in Path at closest distance to Loc

   function Distance (This : Object;
                      Pose : Types.Pose;
                      Path : Map.Path) return Types.Real
                      renames Get_Min_Distance;

   function Is_U_Turn (Path : Map.Path) return Boolean;
   --  Says if this path requires making some U-Turn

   function Contains (Path : Map.Path; Loc : Bit_Location) return Boolean;

   function To_Vector (This : Object;
                       Path : Map.Path) return Sancta.Types.Pose_Vector.Vector;

   function Image (Path : Map.Path) return String;

   function Direction (L1, L2 : Bit_Location) return Directions;
   --  For adjacent locations!

   overriding
   function Hash (This : Object) return String;
   --  For caching.

   overriding
   procedure Finalize   (This : in out Object);

   overriding
   procedure Draw
     (This    :        Object;
      D       : in out Agpl.Drawing.Drawer'Class);

private

   overriding
   procedure Adjust     (This : in out Object);

   pragma Inline (Is_Known, Get_At, Set_At);

   type Matrix_Access is access all Matrix;

   type Cost_Matrix is array (Ordinate range <>, Abscissa range <>,
                              Ordinate range <>, Abscissa range <>) of Costs;
   --  First pair is origin, last pair is destination
   --  Wastes half the space...

   type Cost_Matrix_Access is access all Cost_Matrix;

   type Object (Vicinity : Vicinities) is new Map.Object with record
      Bself     : Object_Access := Object'Unchecked_Access;
      Data      : Matrix_Access;
      Cost      : Cost_Matrix_Access; -- Cached costs between any two locations
      Cell_Size : Float := 0.0;       -- Force invalid default
   end record;

   function Stage_Location (Map  : Sancta.Map.Object'Class;
                            Row  : Rows; Col : Columns)
                            return Sancta.Map.Location'Class;
   function Stage_Observation (Sample : Agpl.Types.Rgb_Triplet)
                               return   Map.Observation'Class;

   Stage_Loader : constant Loader := (Stage_Location'Access,
                                      Stage_Observation'Access);

   overriding
   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   This   :    out Object);
   for Object'Read use Read;

   overriding
   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    This   : in     Object);
   for Object'Write use Write;

end Sancta.Map.Bitmap;
