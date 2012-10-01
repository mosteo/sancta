with Agpl.Containers.String_Sets;
with Agpl.Containers.String_Vectors;

private with Ada.Containers.Indefinite_Ordered_Maps;

package Sancta.Ctree.Connectivity_Matrix is

   pragma Preelaborate;

   Log_Section : constant String := "nerus.connectivity_matrix";

   type Object is tagged private;

   function Get_Umbral (This : Object) return Link_Qualities;

   procedure Set_Umbral (This   : in out Object;
                         Umbral :        Link_Qualities);

   procedure Set_Link (This   : in out Object;
                       R1, R2 :        String;
                       Q      :        Link_Qualities);
   --  Commutative!

   function Contains (This : Object; R1, R2 : String) return Boolean;

   function Get_Link (This : Object; R1, R2 : String) return Link_Qualities;

   function Get_Robots (This : Object)
                        return Agpl.Containers.String_Sets.Set;
   --  Get all robots in this matrix

   function Get_Leaves (This : Object)
                        return Agpl.Containers.String_Sets.Set;
   --  Is_Leaf = true --> ones.

   function Is_Weak (This : Object; R1, R2 : String) return Boolean;
   --  True for signal qualities >= Umbral

   function Is_Leaf (This : Object; Bot : String) return Boolean;
   --  True for bots with 0 .. 1 weak links

   function Are_Weakly_Linked (This   : Object;
                               R1, R2 : String;
                               Hops   : Positive := Positive'Last)
                               return Boolean;
   --  Says if two robots are linked by at most Hops weak links
   --  Cost O (Hops * |Agents|)

   function Neighbors (This      : Object;
                       Bot       : String;
                       Only_Weak : Boolean := False)
                       return Agpl.Containers.String_Sets.Set;
   --  Weak or any neighbours of a robot
   --  Non-existent links are *not* treated as weak

   function Has_Weak_Links (This : Object;
                            Bot  : String) return Boolean;

   function Weak_Count (This : Object) return Natural;
   --  Count of weak links in this topology

   procedure Make_All_Weak (This : in out Object;
                            Bots :        Agpl.Containers.String_Sets.Set);
   --  Any link of Bots are promoted to weak if they are not,
   --  at umbral+epsilon distance.

   function Same_Topology (L, R : Object) return Boolean;
   --  Weak links must be the same in L and R

   function Same_Topology (L, R : Object;
                           Bot  : String) return Boolean;
   --  Partial comparison, just for a given robot.
   --  It must have the same weak links (or none) to be considered same.

   function Prim (This : Object) return Object;
   --  Return the Prim's minimum spanning tree of given conn. matrix
   function Spanning_Tree (This : Object) return Object renames Prim;

   function Depth_First_Search (This : Object;
                                From,
                                To   : String)
                                return Agpl.Containers.String_Vectors.Vector;
   --  Shortest path (in the hops-sense).
   --  Uses recursion, so not very efficient
   --  and the neighbors functions, so indeed *very* inneficient
   --  And *requires* a tree to work.
   --  From and To will be included in the solution

   function Distance (This : Object; From, To : String) return Natural;
   --  0 to itself, so on. In HOPS

   procedure Print (This : Object);
   --  To stdout

private

   type Key (R1_Length, R2_Length : Natural) is record
      R1 : String (1 .. R1_Length);
      R2 : String (1 .. R2_Length);
   end record;

   function Is_Weak (This : Object; Bots : Key) return Boolean;
   pragma Inline (Is_Weak);

   function "<" (L, R : Key) return Boolean; pragma Inline ("<");

   package Link_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Key, Link_Qualities);

   type Object is new Link_Maps.Map with record
      Umbral : Link_Qualities := 0.0;
   end record;

   function Copy (This : Object) return Object is (This);
   pragma Warning ("Not sure this is enough for it to work");

end Sancta.Ctree.Connectivity_Matrix;
