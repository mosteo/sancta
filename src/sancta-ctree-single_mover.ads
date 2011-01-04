with --  Ada.Containers.Indefinite_Ordered_Maps,
     Agpl.Chronos,
     Sancta.Ctree.Connectivity_Matrix,
     Sancta.Ctree.Team_Tree,
     Sancta.Ctree.Tree_Navigator,
     Sancta.Ctree.Stats,
     Sancta.Agent_Proxy,
     Sancta.Assignment,
     Sancta.Containers,
     Sancta.Map,
     Sancta.Map.Smart,
     Sancta.Tasks,
     Sancta.Tasks.Handle;

use Sancta,
    Sancta.Containers;

package Sancta.Ctree.Single_Mover is

   --  Robot mover, only one branch is attempted at a time.
   --  Beware, robots must be named by "1", "2", and so on

   Log_Section : constant String := "Sancta.Ctree.single_mover";

   type Object (<>) is tagged private;

   type Object_Access is access all Object'Class;

   function Create (Team           : Assignment.Object;
                    Plan           : Tc.Lists.List;
                    Here_Threshold : Costs;
                    Near_Threshold : Costs;
                    M              : Map.Smart.Object) return Object;
   --  Initialize this thing, with all robots as free.
   --  Plan is the tasks in order of execution.

   type Step_Outputs is (None,
                         Mission_Completed,
                         Mission_Failed, -- Some task is unreachable
                         Head_Strained);

   procedure Step (This  : in out Object;
                   Team  :        Assignment.Object;
                   Plan  :        Tc.Lists.List;
                   Tree  :        Tree_Navigator.Object'Class;
                   Links :        Connectivity_Matrix.Object'Class;
                   Alloc :    out Assignment.Object;
                   Event :    out Step_Outputs);
   --  Set the inputs and trigger action, next status computations.
   --  Next may be not valid if current goal is last one.
   --  Done indicates if the goal has been reached.

   function Pending_Tasks (This : Object) return Tc.Lists.List;

   function Team_Tree (This : Object) return Ctree.Team_Tree.Object;

private

   type Robot_States is (Relay, Free);

   type Task_States is (Pending, Done);

   type Relative_Locations is (Here, Near, Far);

   type Robot_Action_Kinds is (Hold, Move);

   type Robot_Action_Details is (Unknown, Root,
                                 Stopped, Stopping,
                                 Advancing, Waiting,
                                 Departing, Backtracking);

   type Robot_Actions (Kind : Robot_Action_Kinds) is record
      Detail : Robot_Action_Details := Unknown;
      case Kind is
         when Hold => null;
         when Move => Goal : Tasks.Handle.Object;
      end case;
   end record;

   type Robot_Context is record
      Bot   : Agent_Proxy.Object;
      State : Robot_States;
      Loc   : Map.Location_Handle.Object;
   end record;
   --  Loc is the last position in the tree of this robot, even if is currently
   --  not there!

   type Context_Array is array (Positive range <>) of Robot_Context;

   type Robot_Inputs is record
      Strained : Boolean;
   end record;

   type Input_Array is array (Positive range <>) of Robot_Inputs;

   type Object (Team_Size : Natural) is tagged record
      Here_Threshold : Costs := 2.0;
      Near_Threshold : Costs := 3.0;
      Bots        : Context_Array (1 .. Team_Size);
      Pending     : Tc.Lists.List;
      Branch      : Map.Path;
      Ancestor    : Map.Location_Handle.Object;
      Task_State  : Task_States;
      Inputs      : Input_Array (1 .. Team_Size);
      M           : Map.Smart.Object;
      Tt             : Ctree.Team_Tree.Object;

      Time_Fwrd      : Duration := 0.0;
      Time_Back      : Duration := 0.0; -- Timers for stats
      Cron           : Agpl.Chronos.Object;

      St             : access Stats.Object := new Stats.Object;
      St_Inited      : Boolean := False;
      St_Printed     : Boolean := False;
   end record;

   type Robot_Roles is (Root, Link, Head);

   function Role (This : Object;
                  Bot  : Positive) return Robot_Roles;

   function Name (This : Object;
                  Bot  : Positive) return String;

   function Base (This : Object) return Robot_Context;

   function Go_Fwrd (This : Object;
                     Loc  : Map.Location'Class) return Tasks.Handle.Object;

   function Go_Back (This : Object;
                     Loc  : Map.Location'Class) return Tasks.Handle.Object;
   --  Used to later distinguish, at statistics, backtracking time

   function Dist (This : Object;
                  I, J : Positive) return Relative_Locations;

   function Replegated (This : Object) return Boolean;

   procedure Update_Stats (This : in out Object);
   procedure Print_Stats (This : in out Object);

end Sancta.Ctree.Single_Mover;
