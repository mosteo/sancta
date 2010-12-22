 

with Agpl.Cr.Mutable_Assignment.Moves;

package Agpl.Cr.Mutable_Assignment.Auctions is

   pragma Elaborate_Body;

   --  use UNDO_MOVE_TASK for all moves in this package

   --  O (log)
   procedure Do_Auction_Task (This : in out Object;
                              Undo :    out Undo_Info);
   --  As undo, use the Undo_Move_Task
   --  Cost is kept logaritmic checking only a log fraction of all insertion points.

   procedure Do_Guided_Auction_Task (This : in out Object;
                                     Undo :    out Undo_Info);
   --  Guided in originating agent
   --  As undo, use the Undo_Move_Task

   --  O (n)
   procedure Do_Exhaustive_Auction_Task (This : in out Object;
                                         Undo :    out Undo_Info);
   --  As undo, use the Undo_Move_Task
   --  Will try all possible insertions

   procedure Undo_Move_Task (This : in out Object; Undo : in  Undo_Info)
     renames Moves.Undo_Move_Task;
   --  Will un-move all movements, in the Undo_Info stack, not just one.

end Agpl.Cr.Mutable_Assignment.Auctions;
