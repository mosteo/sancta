with Ada.Containers.Vectors;
with Ada.Streams;
with Agpl.Drawing;
with Agpl.Ustrings;

package Sancta.Map.Qtree is

   --  A quad-tree partition of space.

   Log_Section : constant String := "sancta.map.qtree";
   Det_Section : constant String := "sancta.map.qtree.detail";

   Default_Map_Id : constant String := "default map";

   type Vicinities is (Vicinity_4, Vicinity_6, Vicinity_8);
   --  Vicinity_6 is a mix of 4 and 8, were corners are navigated around with
   --  4-vicinity, but open spaces are 8-vicinity

   type Draw_Modes is (Skel, Skel_With_Neighbors, Solid, Solid_With_Skel);

   type Extra_Options is record
      Draw_Mode  : Draw_Modes := Skel;
      Lazy_Split : Boolean    := False;
      --  Map is only split when a path is requested, and at that point only
      --  across the path obtained to the necessary deep only
   end record;

   type Object (Vicinity : Vicinities) is new Map.Object with private;
   type Object_Access is access all Object;

   type Terrains is (Free, Mixed, Obstacle);

   type Cell_Coords is record
      Xl, Xr : X_Real;
      Yb, Yt : Y_Real;
   end record;

   --  CREATOR
   --  See child package Qtree.Builder for some predefined instances
   generic
      with function Terrain (Xl, Xr : X_Real;
                             Yb, Yt : Y_Real) return Terrains;
      --  This function must inspect the whole range in Xl-Xr, Yb-Yt
      --  Somehow converting it to a source-specific domain
      --  It must also be prepared to be queried outside of the domain
   procedure Create
     (This : in out Object;
      Xmin,
      Xmax : X_Real;
      Ymin,
      Ymax : Y_Real;
      Id   : String := Default_Map_Id);
   --  Id is for serialization of locations across nodes that know the same map

   not overriding
   procedure Set_Options (This : in out Object;
                          Opts :        Extra_Options);

   not overriding
   procedure Set_Cell_Size (This : in out Object;
                            Min,
                            Max  :        Real);
   pragma Precondition (Min <= Max and Min > 0.0 and Max > 0.0);
   --  Here we refer to the cell sizes we want (minimums and maximums)

   type Location is new Map.Location and Agpl.Drawing.Drawable with private;

   not overriding
   function Coords (L : Location) return Cell_Coords;

   overriding
   function "=" (L, R : Location) return Boolean;

   overriding
   function "<" (L, R : Location) return Boolean;

   overriding
   function Image (This : Location) return String;

   overriding
   function Get_Map (This : Location) return access Map.Object'Class;

   overriding
   function Num_Neighbors (L : Location) return Natural;

   overriding
   function Neighbor (L : Location; I : Positive) return Location;

   overriding
   function Is_Neighbor (L, R : Location) return Boolean;

   overriding
   function Real_Cost (Ini, Fin : Location) return Costs;
   --  Real cost of traversing from Ini to Fin

   overriding
   function Estimate_Cost (Ini, Fin : Location) return Costs;
   --  An optimistic estimation of cost between ini and fin

   Unknown_Map : exception;
   --  May be raised on Location'Read, if it is for a map we don't yet have

   not overriding
   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   This   :    out Location);
   for Location'Read use Read;

   not overriding
   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    This   : in     Location);
   for Location'Write use Write;

   type Relative_Position (Pos : Positive) is new Map.Relative_Position
   with null record;

   overriding
   function "<" (L, R : Relative_Position) return Boolean;
   pragma Inline ("<");

   overriding
   function Image (This : Relative_Position) return String;
   pragma Inline (Image);

   overriding
   function Position (Here, There : Location) return Map.Relative_Position'Class;
   --  Just an index into the neighbors...

   type Observation (Terrain : Terrains) is
     new Map.Observation with null record;
   --  Data we want to store in it.

   overriding
   function Is_Traversable (This : Observation) return Boolean;
   pragma Inline (Is_Traversable);

   function Is_Traversable (This : Map.Observation'Class) return Boolean;

   overriding
   function Nearest_Location (This : Object;
                              Pose : Types.Pose) return Map.Location'Class;
   --  Somewhat expensive, O (depth)

   overriding
   function Nearest_Pose (This : Object;
                          Loc  : Map.Location'Class) return Types.Pose;
   --  Middle point of a given cell

   overriding
   function Best_Path (This : Object;
                       Ini,
                       Fin  : Map.Location'Class) return Path_With_Cost;

   not overriding
   procedure Draw_Skel
     (This    :        Object;
      D       : in out Agpl.Drawing.Drawer'Class);
   --  Draw free cells as outlines

   not overriding
   procedure Draw_Solid
     (This    :        Object;
      D       : in out Agpl.Drawing.Drawer'Class);
   --  Draw free cells as blocks

   not overriding
   procedure Draw_Skel_With_Neighbors
     (This    :        Object;
      D       : in out Agpl.Drawing.Drawer'Class);
   --  Draw all cells, and neighborhood

   overriding
   procedure Draw
     (This    :        Object;
      D       : in out Agpl.Drawing.Drawer'Class);
   --  For now, it draws free cells...

private

   use Agpl.Ustrings;

   type Child_Index is (No, Ne, Se, So);

   type Cell_Type;

   type Cell_Access is access all Cell_Type;

   type Child_Array is array (Child_Index) of Cell_Access;

   package Neigh_Vectors is new Ada.Containers.Vectors (Positive, Cell_Access);
   subtype Neigh_Vector is Neigh_Vectors.Vector;

   type Cell_Type is tagged record
      Qmap : Object_Access;

      Terr : Terrains;

      Depth : Positive;    -- Root will be 1
      Index : Child_Index; -- Root will arbitrarily Cell_Index'First

      Xl, Xr : X_Real; -- Xleft, Xright
      Yb, Yt : Y_Real; -- Ytop,  Ybottom

      Parent          : Cell_Access;
      Children        : Child_Array;
      Neighbors       : Neigh_Vector;
      --  Depth_Neighbors : Neigh_Array; -- Points to same-depth neighbors
   end record;

   function Contains (Cell : not null access Cell_Type;
                      Pose :                 Types.Pose) return Boolean;
   pragma Inline (Contains);

   function Has_Children (Cell : not null access Cell_Type) return Boolean;
   pragma Inline (Has_Children);

   function Is_Neighbor (C1, C2 : not null access Cell_Type) return Boolean;

   function Loc (Cell : not null access Cell_Type) return Location'Class;
   function Obs (Cell : not null access Cell_Type) return Observation'Class;
   pragma Inline (Loc, Obs);

   function Nearest_Cell (Cell : not null access Cell_Type;
                          Pose :                 Types.Pose) return Cell_Access;

   generic
      with function Terrain (Xl, Xr : X_Real;
                             Yb, Yt : Y_Real) return Terrains;
   procedure Split (This      :          in out Object;
                    Cell      : not null access Cell_Type'Class;
                    Recursive : Boolean := True);
   --  The "meat": split the cell if needed, recurse into children if asked.
   --  Do this while keeping up-to-date neighbouring information.

   type Location is new Map.Location and Agpl.Drawing.Drawable with record
      Cell : Cell_Access;
   end record;

   overriding
   procedure Draw (This :        Location;
                   D    : in out Agpl.Drawing.Drawer'Class);

   type Object (Vicinity : Vicinities) is new Map.Object
   with record
      QSelf      : Object_Access := Object'Unchecked_Access;
      Opts       : Extra_Options;

      Root       : Cell_Access;

      --  Cell allowed sizes (side)
      Cell_Min   : Real := 1.0;
      Cell_Max   : Real := Real'Last;

      Id         : Ustring := +Default_Map_Id;
   end record;

   procedure Merge_Neighbors (This : in out Object);

   procedure Make_Neighbors (C, D : not null access Cell_Type);

   procedure Test_And_Make_Neighbors (C, D  : not null access Cell_Type;
                                      Valid : Terrains := Free);
   --  Test two cells for neighborhood, and if so make them so
   --  Any Cell.Terr <= Valid is accepted

   procedure Merge_Siblings (Cell : not null access Cell_Type);
   --  Recursively mark neighborhoods of cells for both C and D
   --  Restricted to immediate siblings within this cell

   procedure Remove_From_Neighbors (Cell : not null access Cell_Type);

end Sancta.Map.Qtree;
