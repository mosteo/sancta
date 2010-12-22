with Agpl.Trace;

use Agpl.Trace;

package body Sancta.Component.One_Shot is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Thix : in out Object_Preparer)
   is
      This   :          Object renames Thix.Parent.all;
      Inputs : constant Internal_Key_Array := Object'Class (This).Inputs;
   begin
      if This.Exists (Inputs) then
         Log ("Processing on start", Debug, Log_Section);
         This.Done := True;
         Object'Class (This).Process;
      else
         Log ("Missed on start", Debug, Log_Section);
         for I in Inputs'Range loop
            This.Subscribe (Inputs (I).all);
         end loop;
      end if;
   end Initialize;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      pragma Unreferenced (Key, Value);
   begin
      if (not This.Done) and then This.Exists (Object'Class (This).Inputs) then
         Log ("Processing on callback", Debug, Log_Section);
         This.Done := True;
         Object'Class (This).Process;
      end if;
   end Key_Stored;

end Sancta.Component.One_Shot;
