 

--  A Plan is viable if all its tasks are primitive.

with Sancta.Method;
with Sancta.Method.Vectors;
with Sancta.Plan_Node;
with Sancta.Tasks;
with Sancta.Tasks.Containers;

with Ada.Finalization;
with Ada.Streams;

package Sancta.Plan is

   pragma Preelaborate;

   Log_Section    : constant String := "Sancta.plan";
   Detail_Section : constant String := "Sancta.plan.detail";

   type Object is tagged private;

   function Empty_Plan return Object;
   pragma Inline (Empty_Plan);

   subtype Node_Kind is Plan_Node.Node_Kind;
   subtype Subplan   is Plan_Node.Node_Access;
   --  Renamings used for simplicity.

   function "=" (L, R : in Object) return Boolean;

   procedure Add_Subplan
     (This : in out Object;
      Comp : in     Subplan;
      Kind : in     Node_Kind := Plan_Node.And_Node);
   --  An And/Or tree to be solved.
   --  It's And/Or-ed with any previous tasks as Kind says.

   procedure Add_Method
     (This : in out Object; The_Method : in Method.Object'Class);
   --  Add a method for this plan tasks.

   procedure Add_Task
     (This     : in out Object;
      The_Task : in Tasks.Object'Class;
      Kind     : in Node_Kind := Plan_Node.And_Node);
   --  A task we want to be solved
   --  It's And/Or-ed with any previous tasks as Kind says.

   function Copy_But_Tasks (This : in Object) return Object;
   --  Copy all about a plan but is tasks.

   procedure Enumerate_Tasks
     (This           : in     Object;
      Tasks          :    out Sancta.Tasks.Containers.Lists.List;
      Compound       : in     Boolean := False;
      Primitive      : in     Boolean := False;
      Finished       : in     Boolean := False;
      Pending        : in     Boolean := False);

   function Enumerate_Tasks
     (This           : in     Object;
      Compound       : in     Boolean := False;
      Primitive      : in     Boolean := False;
      Finished       : in     Boolean := False;
      Pending        : in     Boolean := False)
      return                  Sancta.Tasks.Containers.Lists.List;

   procedure Clear (This : in out Object);
   --  Delete everything

   procedure Expand (This    : in Object;
                     Results : not null access procedure (Found : in Object));
   --  Will produce eventually all valid expansions of the plan.
   --  Results is called for each one.
   --  No OR nodes will appear in this plan

   function Expand (This : in Object) return Object;
   --  Will produce a tree of plans where the root is an OR node,
   --  and each child of this OR node is a fully expanded plan.
   --  Will return an empty plan if no valid expansions are found.

   function Contains (This : in Object; Id : in Tasks.Task_Id) return Boolean;
   --  Say if a particular task is somewhere in the plan

   function Get_Node
     (This : in Object;
      Id   : in Tasks.Task_Id) return Subplan;
   --  Get the branch starting at the given task.
   --  May be null if the task isn't in the tree

   function Get_Root (This : in Object) return Subplan;
   --  Get the root node.

   function Get_Task
     (This : in Object;
      Id   : in Tasks.Task_Id) return Tasks.Object_Access;
   --  Gets a reference to a task in this plan. Should not be kept but just
   --  be used puntually.

   function Get_Tasks (This : in Object) return Tasks.Containers.Lists.List;
   --  Obtain a list of the tasks to be performed in this plan
   --  in no particular order.
   --  Task Nodes marked as finished are not reported.
   --  If it is not a valid plan, tasks that would not be both performed (in
   --  OR branches) will be reported nonetheless.

   function Inflate (This : in Object) return Object;
   --  Will take a plan with unexpanded OR nodes and create the expansions.
   --  So the result will be a plan fully expanded, but containing OR nodes.
   --  To get actual executable plans (without OR nodes) see Expand.

   function Is_Empty (This : in Object) return Boolean;
   --  True for plans without tasks (even with methods)

   function Is_Modified (This : in Object) return Boolean;
   --  Says if the plan has been modified.

   procedure Fill_Finished (This : in out Object);
   --  Will mark as finished any tasks depending on OR nodes already finished.

   procedure Mark_Task_Done
     (This : in out Object;
      Id   : in     Tasks.Task_Id;
      Done : in     Boolean := True);
   --  Mark as done this task an any ORed depending on it

   procedure Mark_Task_Done
     (This    : in out Object;
      Id      : in     Tasks.Task_Id;
      Results : not null access procedure (Found : in Object));
   --  Will mark that the task is finished. Will notify all its ancestors.
   --  Methods will notified and a possible plan remodellation can occur.
   --  These results should be re-expanded at a later point.

   procedure Set_Task_Owner
     (This  : in out Object;
      T     : in     Tasks.Task_Id;
      Owner : in     String);
   --  Mark each task node given with its owner.

   procedure Set_Unmodified (This : in out Object);
   --  Cleans the "dirty" bit.

   procedure Print_Summary (This : in Object);
   --  Outputs to Stderr a description of this plan (basicly a list of tasks).

   procedure Print_Tree_Summary (This : in Object);
   --  A more detailed debug dump with tree form and details about each node.

   function Size_In_Bytes (This : in Object) return Natural;

private

   Expansion_Failed : exception;
   --  Raised when a plan is not viable.

   procedure Build_Index (This : in out Object);

   procedure Set_Tasks (This : in out Object; Root : in Subplan);
   --  Set the new tasks in this plan and does maintenance:
   --  Release of previous tasks memory.
   --  Index rebuilding.

   type Object is new Ada.Finalization.Controlled with record
      Methods : Method.Vectors.Vector;
      --  Methods used during plan expansion.

      Index   : Plan_Node.Task_Id_To_Node.Map;
      --  A mapping to directly access task nodes.

      Tasks   : Plan_Node.Node_Access;
      --  The tree of tasks.

      Dirty   : Boolean := False;
      --  To prevent expensive checks, we mark dirty in operations where
      --  the plan is modified.
   end record;

   procedure Adjust   (This : in out Object);
   --  Needed to make deep copies of @Plan_Node@s

   procedure Finalize (This : in out Object);
   --  Free the @Plan_Node@s

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   :    out Object);
   for Object'Read use Read;
   --  Deserialize

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   : in     Object);
   for Object'Write use Write;
   --  Serialize pointers and such.

end Sancta.Plan;
