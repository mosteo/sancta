with Ada.Finalization,
     Sancta.Component.Root;

package Sancta.Component.One_Shot is

   --  Component base that may be used when processing must be performed
   --  only once, when the input keys are available for the first time.

   Log_Section : constant String := "sancta.component.one_shot";

   type Object is abstract new Root.Object with private;

   not overriding
   procedure Process (This : in out Object) is abstract;
   --  Override and do whatever (generating Output).
   --  This is called only when all the inputs are available.

   --  * HUGE WARNING -- IF PROCESS IS CALLED ON INITIALIZATION BECAUSE ALL
   --  INPUTS ARE ALREADY AVAILABLE, THEN REMEMBER THAT COMPONENT VARS CONSTRUCTION
   --  IN THE CHILD CLASS WILL BE PERFORMED *AFTER* PROCESS, POTENTIALLY
   --  OVERWRITING ANY CHILD COMPONENT VARIABLES.
   --  I SHOULD DO SOMETHING ABOUT THISSSSS - If I knew what

   not overriding
   function Inputs (This : Object) return Internal_Key_Array is abstract;
   --  Inputs to be monitored; they're automatically subscribed so you don't
   --  need to do so.

private

   type Object_Preparer (Parent : access Object) is limited new
     Ada.Finalization.Limited_Controlled with null record;

   procedure Initialize (Thix : in out Object_Preparer);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   type Object is abstract new Root.Object with
      record
         Done : Boolean := False;
         Prep : Object_Preparer (Object'Access);
      end record;

end Sancta.Component.One_Shot;
