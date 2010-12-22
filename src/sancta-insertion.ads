with Sancta;
with Sancta.Agent;
with Sancta.Assignment;
with Sancta.Containers;
with Sancta.Cost_Cache;
with Sancta.Criteria; use Sancta.Criteria;
with Sancta.Tasks;
with Sancta.Tasks.Handle;
with Agpl.Ustrings;             use Agpl.Ustrings;
with Agpl; use Agpl;

use Sancta.Containers;

--  Task allocation with insertion heuristic

package Sancta.Insertion is

   pragma Preelaborate;

   pragma Buggy ("Some results of using this are very strange, do not use for now!");


   Two_Opt_Enabled : Boolean := False;

   type Bids is tagged private;

   function "<" (L, R : Bids) return Boolean;
   pragma Inline ("<");

   function Can_Win (Bid : Bids) return Boolean;
   pragma Inline (Can_Win);
   --  Bids with Infinite or no task cannot win

   function Task_Id (Bid : Bids) return Sancta.Tasks.Task_Id;
   pragma Inline (Task_Id);
   --  Id associated with a bid, could be No_Task

   function Get_Agent (Bid : Bids) return String;
   --  Winner's name

   function Get_Cost (Bid : Bids) return Costs;
   pragma Inline (Get_Cost);
   --  Cost of winning bid

   function Get_Task (Bid : Bids) return Sancta.Tasks.Object'Class;
   --  Task being added, could be No_Task

   function Apply (Ass : Assignment.Object;
                   Bid : Bids) return Assignment.Object;
   --  Apply a bid to an assignment.

   function Apply (Agent : Sancta.Agent.Object'Class;
                   Bid   : Bids) return Sancta.Agent.Object'Class;
   --  Apply to an agent, bid must have been created by the given agent

   function Auction (Ass   : Assignment.Object;
                     Tasks : Tc.Lists.List;
                     Crit  : Assignment_Criteria;
                     Cost  : Cost_Cache.Object'Class) return Bids;
   --  Best bid, given an assignment.

   function Auction (Agent : Sancta.Agent.Object'Class;
                     Tasks : Tc.Lists.List;
                     Crit  : Assignment_Criteria;
                     Cost  : Cost_Cache.Object'Class) return Bids;
   --  Best bid, given an agent

   function Auction (Agent      : Sancta.Agent.Object'Class;
                     Job        : Sancta.Tasks.Object'Class;
                     Crit       : Assignment_Criteria;
                     Cost       : Cost_Cache.Object'Class;
                     Use_Cached : Boolean := True) return Bids;
   --  Best bid, given a single task-agent pair
   --  Tries insertion and 2-opt if enabled

   function Two_Opt
     (Old        : Sancta.Agent.Object'Class;
      Agent      : Sancta.Agent.Object'Class;
      Job        : Sancta.Tasks.Object'Class;
      Crit       : Assignment_Criteria;
      Cost       : Cost_Cache.Object'Class;
      Use_Cached : Boolean := True) return Bids;
   --  Old must not contain Job, and Agent must
   --  Job is the id of the task just added.

private

   type Bid_Kind is (Invalid, Insertion, Full_List);

   type Bids is tagged record
      Cost   : Sancta.Costs := Sancta.Infinite;
      Bot    : Ustring;
      Job    : Sancta.Tasks.Handle.Object;

      Kind   : Bid_Kind := Invalid;
      Before : Sancta.Tasks.Task_Id;
      List   : Tc.Lists.List;
   end record;

   function Evaluate_Bid (Old_Agent,
                          New_Agent  : Sancta.Agent.Object'Class;
                          Crit       : Assignment_Criteria;
                          Cost       : Sancta.Cost_Cache.Object'Class;
                          Use_Cached : Boolean := True)
                          return      Costs;

end Sancta.Insertion;
