 

--  The difference with Sancta.Mutable_assignment is that that one used several
--  hacks for the problem we had at hand at that time.

--  This one strives to be a really general, problem-independent solution.

package Agpl.Cr.Mutable_Assignment.Or_Mutations is

   pragma Elaborate_Body;

   procedure Do_Switch_Or_Node (This : in out Object;
                                Undo :    out Undo_Info);
   procedure Undo_Switch (This : in out Object; Undo : in Undo_Info);

end Agpl.Cr.Mutable_Assignment.Or_Mutations;
