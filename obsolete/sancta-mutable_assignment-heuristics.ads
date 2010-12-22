 

package Agpl.Cr.Mutable_Assignment.Heuristics is

   pragma Elaborate_Body;

   procedure Undo_From_Scratch (This : in out Object; Undo : in Undo_Info)
     renames Mutable_Assignment.Undo_From_Scratch;
   --  Undo for all heuristics

   procedure Do_Heuristic_1 (This : in out Object;
                             Undo :    out Undo_Info)
     renames Mutable_Assignment.Do_Heuristic_1;
   --  Will consider all agents and tasks to provide some "good" assignment.
   --  The current tasks are re-assigned in a "best pair" greedy fashion.
   --  So no OR node switchings happen.

   procedure Do_Heuristic_2 (This : in out Object;
                             Undo :    out Undo_Info);
   --  This heuristic will consider the best of *all* tasks in every possible
   --  expansion; freeze the plan with the chosen task; repeat until no more T.

   --  O (n^2)
   procedure Do_Agent_Reorder (This : in out Object;
                               Undo :    out Undo_Info);
   --  Greedy reordering of an agent tasks

end Agpl.Cr.Mutable_Assignment.Heuristics;
