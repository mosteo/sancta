--  Abstract root for mapping classes.
--  We can have an underlying grid map, vectorial, etc.

with Ada.Containers.Doubly_Linked_Lists,
     Ada.Containers.Indefinite_Doubly_Linked_Lists,
     Ada.Containers.Indefinite_Ordered_Maps,
     Ada.Containers.Indefinite_Ordered_Sets,
     Ada.Containers.Indefinite_Vectors,
     Ada.Finalization,
     Ada.Streams;

with Agpl.Containers.Algorithms,
     Agpl.Drawing,
     Agpl.Generic_Handle,
     Agpl.Types;

with Sancta.Containers,
     Sancta.Types;

use Sancta.Types;

package Sancta.Map is

   --   pragma Preelaborate;
   --   Zlib and Png_Io prevent this to be preelaborable

   Log_Section    : constant String := "sancta.map";
   Detail_Section : constant String := "sancta.map.detail";

   No_Data : exception;

   type Object is
     new Ada.Finalization.Limited_Controlled
     and Agpl.Drawing.Drawable with private;
   --  The map itself.

   type Object_Access is access all Object'Class;

   type Relative_Position is abstract tagged null record;

   function "<" (L, R : Relative_Position) return Boolean is abstract;
   --  Arbitrary well-defined ordering, for use in ordered containers.

   function Image (This : Relative_Position) return String is abstract;

   function Class_Less_Than (L, R : Relative_Position'Class) return Boolean;
   function Class_Image     (This : Relative_Position'Class) return String;

   type Location is abstract tagged null record;

   function "=" (L, R : Location) return Boolean is abstract;

   function "<" (L, R : Location) return Boolean is abstract;

   function Image (This : Location) return String;
   --  Defaults to external tag; should be overriden so it's unique to each
   --  location. Otherwise some algorithms (e.g. A*) will fail.

   function Get_Map (L : Location) return access Object'Class is abstract;
   --  BRUAUUAUUAUAUU

   function Num_Neighbors (L : Location) return Natural is abstract;

   function Neighbor (L : Location; I : Positive) return Location is abstract;

   function Is_Neighbor (L, R : Location) return Boolean is abstract;

   function Real_Cost (Ini, Fin : Location) return Costs is abstract;
   --  Real cost of traversing from Ini to Fin

   function Estimate_Cost (Ini, Fin : Location) return Costs is abstract;
   --  An *optimistic* estimation of cost between ini and fin.
   --  Used in A*

   function Position (Here, There : Location) return Relative_Position'Class
   is abstract;
   --  Say where is There, from Here.

   package Location_Handle is new Agpl.Generic_Handle (Location'Class);

   type Observation is abstract tagged null record;
   --  Data we want to store in it.

   function Is_Traversable (This : Observation) return Boolean is abstract;

   function Is_Known (This : in Object;
                      Loc  : in Location'Class) return Boolean;
   --  Returns true if there's any observation associated with the coordinates.

   function Get_At (This : in Object;
                    Loc  : in Location'Class) return Observation'Class;
   --  Get data at some coords.
   --  May raise No_Data if nothing already there.

   procedure Set_At (This : in out Object;
                     Loc  : in     Location'Class;
                     Obs  : in     Observation'Class);
   --  Set/Merge information at given coords.
   --  This default replaces the current observation

   not overriding
   function Nearest_Location (This : Object;
                              Pose : Types.Pose) return Location'Class;
   --  Default Program_Error

   function Nearest_Location (This : Object'Class;
                              P    : Types.Point) return Location'Class;
   --  Dispatches to the Pose version

   not overriding
   function Nearest_Pose (This : Object;
                          Loc  : Location'Class) return Types.Pose;
   --  Default Program_Error

   type Loader is record
      Loc : not null access function (Map : Object'Class;
                                      Row : Rows; -- The ones in the PNG file
                                      Col : Columns) return Location'Class;
      Obs : not null access function (Sample : Agpl.Types.Rgb_Triplet) return Observation'Class;
   end record;

   procedure From_Png (This : in out Object;
                       File :        String;
                       Load :        Loader);
   --  Load from a file.

   package Location_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Location'Class);

   package Paths renames Location_Lists;

   subtype Path is Paths.List;

   Empty_Path : Path renames Location_Lists.Empty_List;

   type Path_With_Cost is record
      Path : Map.Path;
      Cost : Costs;
   end record;

   function Image (P : Path) return String;

   function Is_Equal (L, R : Location'Class) return Boolean;
   --  Dispatchs to "="

   package Path_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Path, Paths."=");

   package Location_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Location'Class);

   package Path_Algs is
     new Agpl.Containers.Algorithms.Basic
       (Path,
        Location'Class,
        Paths.Cursor,
        Paths.Element);

   package Path_Fields is
     new Path_Algs.Fields (Paths.Has_Element,
                           Paths.First, Paths.Last,
                           Paths.Next, Paths.Previous,
                           Paths.Append);

   function To_Vector (This : Location_Lists.List)
                       return Location_Vectors.Vector;

   function To_Poses (This : Object;
                      Path : Map.Path)
                      return Sancta.Containers.Pose_Vectors.Object;

   function Less_Than (L, R : Location'Class) return Boolean;
   --  For internal purposes...

   function Depth (L : Location'Class; P : Path) return Positive;
   --  number of locations since the first in P to L

   function Remove_Head (P : Path; Cells : Natural) return Path;

   procedure Append is new Path_Algs.Append (Paths.Append, Paths.Iterate);

   package Loc_Cost_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Location'Class, Costs, Less_Than);

   type Neighbor_Function is access
     function (M : Map.Object'Class;
               L : Location'Class)    return Location_Vectors.Vector;

   function Best_Path (This : Object;
                       Ini,
                       Fin  : Location'Class) return Path_With_Cost;
   --  Uses A*
   --  May raise Constraint_Error if no route

   function Best_Path (This : Object;
                       Ini,
                       Fin  : Types.Pose) return Path_With_Cost;
   --  As previous, uses the nearest locations

   not overriding
   function Hash (This : Object) return String;
   --  For caching. Default raises program error.
   --  Should be unique for each map configuration

   function Hash (This : Object'Class) return String;
   --  Dispatchs to internal one

   not overriding
   function Get_Cost_Between (This : Object;
                              Ini,
                              Fin  : Location'Class) return Costs;
   --  The cost of traversing from Ini to Fin
   --  Default dispatches on Real_Cost (Ini, Fin)
   --  Also, default caches the costs computed to avoid recalculations.

   not overriding
   procedure Compute_Costs (This : in out Object);
   --  Should compute/actualize costs between all map locations.
   --  This default raises program error

   function Steps (Ini,
                   Fin     : Location'Class;
                   Through : Path) return Natural;
   --  Locations to be traversed across Path to reach Fin from Ini

   function Get_Cost_Between
     (This : Object;
      Ini,
      Fin  : Types.Pose) return Costs;
   --  Cost between two poses, but following the path:
   --  Euclid. distance for neighbor locations,
   --  Rounded cost between locations cc.

   function Get_Cost_Across_Path
     (This    : Object;
      Ini,
      Fin     : Types.Pose;
      Route   : Path) return Costs;
   --  Distance between poses, going across a particular route.
   --  if Nearest_Location (Ini) /= Route.First then we add that distance.
   --  Likewise for Fin

   function Path_Cost (This  : Object;
                       Route : Path) return Costs;
   --  Get the Route real cost calling Real_Cost across neighbors

   function Next (Route : Path;
                  From  : Location'Class) return Location'Class;
   --  Get the next location to a given one, or last again

   function Prev (Route : Path;
                  From  : Location'Class) return Location'Class;
   --  Get the prev location to a given on, or first again

   function Is_Before (Route : Path;
                       Loc   : Location'Class;
                       Than  : Location'Class) return Boolean;
   --  Says if Loc is before Than in Path
   --  False if Loc = Than

   function Prefix (Route : Path;
                    Loc   : Location'Class) return Path;
   --  Get Route until loc is found, or full route if not found.
   --  Loc is not included, in any case.

   function Tail (Route : Path;
                  Loc   : Location'Class) return Path;
   --  tail of route after last appearance of Loc, whole route if not present

   function Common_Ancestor (X, Y : Path) return Location'Class;
   --  Returns the last common location; at least first one should be equal!
   --  It assumes that once a path diverges from the other they never cross again

   function Common_Ancestor (X, Y : Path) return Paths.Cursor;
   --  As previous, but returns a cursor from X

   function Last_Y_Location_In_X (X, Y : Path) return Location'Class;
   --  Returns the last location in Y that's also in X.
   --  If none common, constraint error

   function Nearest_Location_In_Path (M    : Object;
                                      To   : Location'Class;
                                      From : Path) return Location'Class;

   function Nearest_Location_In_Path_From_Path_End
     (M    : Object;
      To   : Location'Class;
      From : Path) return Location'Class;
   --  Returns the location in From that gives best cost, computing:
   --    Cost (from.last, best_loc) + Cost (best_loc, to)

   function Nearest_Location_In_Path_Estimated
     (M    : Object;
      To   : Location'Class;
      From : Path) return Location'Class;
   --  Uses the Estimate_Cost instead of real best, for speed.

   type Endpoints is record
      Ini,
      Fin : Location_Handle.Object;
   end record;

   function "<" (L, R : Endpoints) return Boolean;

   not overriding
   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   This   :    out Object) is null;

   not overriding
   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    This   : in     Object) is null;

   --  DEBUGGGG

   procedure Print (This : Object);

   overriding
   procedure Draw (This : Object; D : in out Agpl.Drawing.Drawer'Class) is null;

private

   --  Default implementation is a sorted map.
   package Loc_Obs_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Location'Class, Observation'Class, Less_Than);

   package Ends_Costs_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Endpoints, Costs);

   package Location_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Location'Class, Less_Than);

   type Object is
     new Ada.Finalization.Limited_Controlled
     and Agpl.Drawing.Drawable
   with record
      Self       : Object_Access := Object'Unchecked_Access;
      Loc_Obs    : Loc_Obs_Maps.Map;
      Cost_Cache : Ends_Costs_Maps.Map;
   end record;

   --  Just to have a null location:
--     type Null_Location_Type is new Location with null record;
--     function "<" (L, R : Null_Location_Type) return Boolean;
--     Null_Location : constant Location'Class := Null_Location_Type'(null record);

   procedure Class_Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                          This   : in     Object'Class);
   for Object'Class'Write use Class_Write;

end Sancta.Map;
