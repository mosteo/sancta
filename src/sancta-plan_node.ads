 

--  Plan points are subtrees of actions to be performed inside a Plan.
--  Methods create plan points which will be hooked under the Composite Tasks
--  which have been expanded by the Method.

--  with Agpl.Debug;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Streams;
with Ada.Unchecked_Deallocation;

package Sancta.Plan_Node is

   --  It should ideally be a private package but to avoid a level of indirection
   --  we'll omit that. Clients not using it via Plan can't do anything with it
   --  anyway.

   pragma Preelaborate;

   Detail_Section : constant String := "Sancta.plan_node.detail";

   type Object (<>) is limited private;
   --  To prevent creation outside of our control.

   type Node_Access is access Object;
   --   for Node_Access'Storage_Pool use Agpl.Debug.Pool;

   subtype Node_Id is String;

   function Equivalent (L, R : in Node_Access) return Boolean;
   --  Compares task bodies and node ids

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   :    out Node_Access);
   for Node_Access'Read use Read;
   --  Deserialize

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   : in     Node_Access);
   for Node_Access'Write use Write;
   --  Serialize pointers and such.

   type Node_Array is array (Positive range <>) of Node_Access;

   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists (Node_Access);
   package Node_Vectors is new Ada.Containers.Vectors (Positive, Node_Access);
   package Task_Id_To_Node is new Ada.Containers.Ordered_Maps
     (Tasks.Task_Id, Node_Access, Tasks."<");

   type Node_Kind is (And_Node, Or_Node, Task_Node);
   subtype Composite_Node_Kind is Node_Kind range And_Node .. Or_Node;

   procedure Append_Child (This, Child : Node_Access);
   --  Append a child node to a node.
   --  If the node was task, its appended to its expansion (must exist).
   --  If the node was AND or OR, the child is appended to the list.

   procedure Build_Index
     (Node  : in     Node_Access;
      Index : in out Task_Id_To_Node.Map;
      Clean : in     Boolean := True);
   --  Recreates the index. If @Clean@, @Index@ is cleared at start.

   function Create (From   : in Tasks.Object'Class;
                    Parent : in Node_Access := null) return Node_Access;
   --  Allocates a node consisting of a single task.

   function Create
     (Kind   : in Composite_Node_Kind;
      From   : in Tasks.Containers.Lists.List;
      Parent : in Node_Access := null) return Node_Access;
   --  Allocates a node consisting in several and/or tasks

   function Create
     (Kind   : in Composite_Node_Kind;
      L, R   : in Node_Access;
      Parent : in Node_Access := null) return Node_Access;
   --  Binary tree likewise composition.
   --  If some of these is null, it will not be added.
   --  L.Parent and R.Parent are set to the newly created node.

   function Create
     (Kind   : in Composite_Node_Kind;
      Nodes  : in Node_Lists.List;
      Parent : in Node_Access := null) return Node_Access;
   --  Multi-leaf tree composition.

   function Create
     (Kind   : in Composite_Node_Kind;
      Nodes  : in Node_Vectors.Vector;
      Parent : in Node_Access := null) return Node_Access;
   --  Multi-leaf tree composition.

   function Create
     (Kind   : in Composite_Node_Kind;
      Nodes  : in Node_Array;
      Parent : in Node_Access := null) return Node_Access;
   --  Multi-leaf tree composition.

   function Deep_Copy (This   : in Node_Access;
                       Parent : in Node_Access := null) return Node_Access;
   --  Do a deep copy of this node.
   --  Its parent will be replaced with @Parent@.

   function Deep_Copy_With_Replace
     (This     : in Node_Access;
      Old_Node : in Node_Access;
      New_Node : in Node_Access;
      Parent   : in Node_Access) return Node_Access;
   --  Do a deep copy, but replacing the branch starting at Old_Node with
   --  New_Node.
   --  New_Node.Parent will be the corresponding to the deep copy.

   procedure Delete (This : in out Node_Access);
   --  Frees this allocated node.

   procedure Enumerate_Tasks
     (This           : in     Node_Access;
      Tasks          :    out Sancta.Tasks.Containers.Lists.List;
      Compound       : in     Boolean := False;
      Primitive      : in     Boolean := False;
      Finished       : in     Boolean := False;
      Pending        : in     Boolean := False);
   --  Compound/Primitive and Finished/Pending are an AND condition
   --  So you can select Compound AND Finished but not Compound OR Finished

   function Get_Kind (This : in Node_Access) return Node_Kind;

   function Get_Children (This : in Node_Access) return Node_Lists.List;
   --  For And/Or nodes only.

   function Get_Children (This : in Node_Access) return Node_Vectors.Vector;
   --  For And/Or nodes only.

   function Get_Expanded (This : in Node_Access) return Boolean;
   --  For Task nodes only.

   function Get_Expansion (This : in Node_Access) return Node_Access;
   --  Get the expansion child for a task node.

   function Get_Finished (This : Node_Access) return Boolean;
   --  Says *this* node is marked as finished

   procedure Set_Finished (This      : Node_Access;
                           Finished  : Boolean := True;
                           Recursive : Boolean := False);
   --  Mark this node (and descendents optionally).
   --  Still, Fill_Finished is necessary to propagate changes due to nodes
   --  above this one.

   function Get_Id (This : not null Node_Access) return String;
   --  Some unique id. Copied across plans.

   function Get_Owner (This : in Node_Access) return String;
   procedure Set_Owner (This : in Node_Access; Owner : in String);
   --  Just for task nodes.

   function Get_Task (This : in Node_Access) return Tasks.Object'Class;
   function Get_Task (This : in Node_Access) return Tasks.Object_Access;
   --  For Task nodes only.

   procedure Set_Child (This  : in Node_Access;
                        Child : in Node_Access;
                        Force : in Boolean := False);
   --  For Task nodes, this sets the expansion of the node.

   procedure Set_Children (This     : in Node_Access;
                           Children : in Node_Vectors.Vector;
                           Force    : in Boolean := False);
   --  For AND/OR nodes, this replaces the expansion vector
   --  If not force and the node already has children, Constraint_Error.

   function Get_Parent (This : in Node_Access) return Node_Access;
   --  Get immediate parent node.

   function Is_Ancestor (This    : in Node_Access;
                         Of_This : in Node_Access) return Boolean;
   --  Says if a node is child of another at any distance.

   function Get_Parent_Task (This : in Node_Access) return Node_Access;
   --  Get first ancestor that is a task node or null if none.

   procedure Fill_Finished (This : in Node_Access);
   --  Mark as finished any tasks requiring it due to OR nodes.

   function Is_Sane (This   : in Node_Access;
                     Parent : in Node_Access := null) return Boolean;
   --  Debug: Check correctness in parent/child pointers.
   --  Call it with Parent => null from the root Node.

private

   type Object (Kind : Node_Kind) is new Ada.Finalization.Limited_Controlled
   with record
      Parent   : Node_Access; -- Will be null for the root.
      Id       : Ustring;     -- Assigned on initialization.
      Finished : Boolean := False; --  If the task has been already performed.
      --  For OR/AND nodes, it means the descendent dependencies.

      case Kind is
         when Task_Node =>
            The_Task : Tasks.Object_Access;
            Owner    : Ustring;          --  Name of the agent performing it.

            Child    : Node_Access;
            --  Pointer to the expansion of this node.

         when And_Node | Or_Node =>
            Children : Node_Vectors.Vector;
      end case;
   end record;

   procedure Initialize (This : in out Object);
   procedure Finalize   (This : in out Object);

   procedure Delete_Internal is new Ada.Unchecked_Deallocation
     (Object, Node_Access);

end Sancta.Plan_Node;
