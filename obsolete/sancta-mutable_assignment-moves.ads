 

package Agpl.Cr.Mutable_Assignment.Moves is

   pragma Elaborate_Body;

   procedure Undo_Move_Task (This : in out Object; Undo : in  Undo_Info);
   --  Will un-move all movements, in the Undo_Info stack, not just one.

   --  O (log)
   procedure Do_Move_Task (This : in out Object;
                           Undo :    out Undo_Info);

   --  O (log)
   procedure Do_Move_Task_Changing_Owner (This : in out Object;
                                          Undo :    out Undo_Info);
   --  Moves a task at random, but choses the owner before hand. In this way,
   --  no agent can end without tasks (as happens when just using Move_Task
   --  As undo, use the Undo_Move_Task

   procedure Do_Guided_Move_Task_Changing_Owner (This : in out Object;
                                                 Undo :    out Undo_Info);
   --  Like previous, but task is chosen from the worst cost agent

   procedure Do_Swap_Order (This : in out Object;
                            Undo :    out Undo_Info);
   --  Switches two consecutive tasks
   --  As undo, use the Undo_Move_Task

   procedure Do_Swap_Tasks (This : in out Object;
                            Undo :    out Undo_Info);
   --  Switches two arbitrary tasks
   --  As undo, use the Undo_Move_Task

end Agpl.Cr.Mutable_Assignment.Moves;
