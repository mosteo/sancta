with Ada.Command_Line; use Ada.Command_Line;

with Gnat.Os_Lib;
use Gnat;

--  The purpose of this program is to compare cost incurred when tasks are
--  added in subsequent steps after initial solution.

--  In this example, a task is added each time another one is finished.
--  We specify the initial number of tasks and the additional tasks

--  <program> <initial tasks> <additional tasks>

--  There's just one agent performing tasks.

--  In this specific case, the tasks are created in a "cloud" which moves
--  steadily forward...

procedure Exitcode is
begin

   Os_Lib.OS_Exit (Argument_Count);

end Exitcode;
