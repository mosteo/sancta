 

--  Implementation of traderbots for EXPRES.

with Sancta.Auctioner;
with Sancta.Network;
with Sancta.Simplebot2;

with Agpl.Chronos;
with Sancta; use Agpl.Cr;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Primitive;
with Agpl.Xml;
with Agpl; use Agpl;

with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Ordered_Maps;

package Sancta.Traderbot is

--   pragma Elaborate_Body;

   Log_Section    : constant String := "sancta.traderbot";
   Detail_Section : constant String := "sancta.traderbot.detail";
   --  To selectively enable debug messages...

   type Auction_Task_Policies is record
      First                    : Boolean := False;
      Second                   : Boolean := True;
      All_But_First_And_Second : Boolean := True;
      Generated                : Boolean := True;
      Added                    : Boolean := False;
      Won                      : Boolean := False;
   end record;
   --  What task will be auctioned at the actioning time.
   --  Generated: Tasks generated by normal operation.
   --  Added: tasks received from operator.
   --  Won: tasks won in auction.

   Default_Auction_Task_Policy : constant Auction_Task_Policies :=
                                   (First                    => False,
                                    Second                   => True,
                                    All_But_First_And_Second => True,
                                    Generated                => True,
                                    Added                    => False,
                                    Won                      => False);

   type Auction_Time_Policies is record
      Auction_On_Adition      : Boolean  := True; -- When a task is added manually.
      Auction_On_Finalization : Boolean  := True; -- When a task is finished.
      Periodic                : Duration := Duration'Last;
   end record;
   --  When to start an auction

   Default_Auction_Time_Policy : constant Auction_Time_Policies :=
                                   (Auction_On_Adition      => True,
                                    Auction_On_Finalization => True,
                                    Periodic                => Duration'Last);

   --  If Added, just the Added will be auctioned.
   --  If not Added and Auction_On_Addition, all the tasks will be auctioned.

   type Insertion_Policies is (First, Last, Best);
   --  A new task is added to the first, last or best position.
   --  Best causes (|T| + 1) Task-to-Task costs to be evaluated.

   type Object is new Simplebot2.Object with private;

   type Object_Access is access all Object'Class;

   procedure Add_Task (This : in out Object;
                       That : in     Sancta.Tasks.Object'Class;
                       Keep : in     Boolean := False);
   --  Add this task to the robot list.
   --  Insertion place governed by current insertion policy.
   --  Auctioning governed by current policies, unless keep is True and then
   --  the task is forcefully kept.

   procedure Cancel_Task (This : in out Object;
                          Id   : in     Sancta.Tasks.Task_Id);

   procedure Do_Auction (This : in out Object);
   --  Force an auction now.

   procedure Do_Auction (This : in out Object;
                         Id   : in     Sancta.Tasks.Task_Id);
   --  Force auction of a single task now.
   --  Id should be always a primitive task. If there's a compound task above it,
   --  it will be found and auctioned automatically.

   procedure Run (This : in out Object;
                  Done :    out Boolean);
   --  Run a control step. Should be called periodically.
   --  Done will be true when the bot has finished operation.
   --  Once this happens Run shouldn't be called again.

   procedure Set_Configuration
     (This                : in out Object;
      Config              : in     Xml.Document;
      Auction_Duration    : in     Duration              := 0.5;
      Auction_Task_Policy : in     Auction_Task_Policies := Default_Auction_Task_Policy;
      Auction_Time_Policy : in     Auction_Time_Policies := Default_Auction_Time_Policy;
      Insertion_Policy    : in     Insertion_Policies    := Best;
      Cost_Policy         : in     Auctioner.Cost_Policies := Auctioner.Full_Cost;
      Max_Bidders         : in     Natural               := Natural'Last);
   --  Stablish traderbot configuration.
   --  Must be called before starting operation!

private

   Cant_Integrate_Task : exception;

   use type Sancta.Tasks.Task_Id;

   package Id_Id_Maps is new Ada.Containers.Ordered_Maps
     (Sancta.Tasks.Task_Id, Sancta.Tasks.Task_Id);

   type Object is new Simplebot2.Object with record
      Shutdown            : Time                  := Clock + 30.0 * 24.0 * 3600.0;
      --  When the robot has to stop. Initially never.

      Auction_Duration    : Duration              := 0.5;
      Auction_Task_Policy : Auction_Task_Policies := Default_Auction_Task_Policy;
      Auction_Time_Policy : Auction_Time_Policies := Default_Auction_Time_Policy;
      Insertion_Policy    : Insertion_Policies    := Best;
      Cost_Policy         : Auctioner.Cost_Policies := Auctioner.Full_Cost;

      Auctioning_All      : Boolean := False; -- If a full round is going on.
      Auctioning_Pos      : Natural := 1;     -- Current pos being auctioned.

      Auctioning          : Boolean := False;

      Best_Bid            : Costs;
      Best_Bidder         : Network.Node_Id := Network.No_Node;  -- Id of bidder
      Full_Best_Bid       : Costs;
      Full_Best_Bidder    : Network.Node_Id := Network.No_Node;

      Bids_Received       : Natural;          -- Used for early auction end.
      End_Of_Auction      : Time;
      Max_Bidders         : Natural := Natural'Last; -- Known companions.
      Task_To_Auction     : Sancta.Tasks.Task_Id;  -- Always primitive
      --  Even if we are really auctioning his parent.

      Parent_Tasks        : Id_Id_Maps.Map;
      --  Contains the id of the parent task for a given one.
      --  This is used when a task is completed to notify that its parent
      --  has been too completed. This emanates from the current restriction
      --  that a compound task can only be expanded in a single prim task.

      Start_Auction_Timer : Chronos.Object;
      --  Used to force a periodic auction

      Dont_Start_Auction_Timer : Chronos.Object;
      --  If we've been forced to bid (forced tasks) recently, don't start
      --  our own auction for some seconds, since more tasks can be coming,
      --  nor award any running auction not to interfere the forced auction.

      Bidding_Timer        : Chronos.Object;
      --  This is to avoid entering into 2 auctions simultaneously

--        Insertion_Procedure : Sancta.Tasks.Insertions.Insertion_Procedures :=
--          Sancta.Tasks.Insertions.Greedy'Access;
--        --  For now, this is hardwired...
   end record;

   procedure Award_Auction (This : in out Object);
   --  Send the award message.

   procedure Continue_Auction (This : in out Object);
   --  Check received bids, timeout, etc.

   function Evaluate_Best_Plan (This : in Object;
                                Jobs : in Sancta.Tasks.Containers.Lists.List)
                                return    Sancta.Tasks.Containers.Lists.List;
   --  Create greedily a best plan considering task flips
   --  This is an ugly hack, will only flip once a task because we known the
   --  tasks under consideration are alternates.

   function Evaluate_Concorde (This : in Object;
                               Jobs : in Sancta.Tasks.Containers.Lists.List)
                               return    Sancta.Tasks.Containers.Lists.List;

   function Evaluate_Best_Insertion (This : in Object;
                                     Jobs : in Sancta.Tasks.Containers.Lists.List;
                                     Job  : in Sancta.Tasks.Object'Class)
                                     return    Sancta.Tasks.Containers.Lists.List;
   --  Say the best point to insert a task. Task is always expected to be
   --  primitive, its parent and siblings will be automatically generated.

   function Evaluate_Best_Task_List (This : in Object;
                                     Jobs : in Sancta.Tasks.Containers.Lists.List;
                                     Job  : in Sancta.Tasks.Object'Class)
                                     return    Sancta.Tasks.Containers.Lists.List;
   --  Calls the two previous ones and return best result

   function Evaluate_Removing (This : in Object;
                               That : in Sancta.Tasks.Task_Id) return Costs;
   --  Return the cost of the best possible plan without that task

   procedure Init (This : in out Object);

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);
   --  Do something with a received packet.

   overriding
   procedure Task_Finished (This : in out Object;
                            Job  : in out Sancta.Tasks.Primitive.Object'Class);
   --  Overrides Netbot.Task_Finished

end Sancta.Traderbot;
