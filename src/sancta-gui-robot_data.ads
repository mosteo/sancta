--  Data server for communication between robot and gui.

with Sancta.Network;
with Sancta.Types;

with Agpl.Containers.String_String_Maps;
with Sancta;
with Sancta.Tasks;
with Sancta.Tasks.Handle;
with Sancta.Tasks.Containers;
with Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;
with Agpl; use Agpl;

package Sancta.Gui.Robot_Data is

   --  pragma Preelaborate;

   --  Signals defined
   type Signals is (Pose,
                    Tasks,
                    Command);

   --  Attributes defined
   Localized_Pose : constant String := "loc pose";
   Last_Command   : constant String := "last command";
   Velocity       : constant String := "velocity";

   --  Object
   protected type Object is

      function Get_Attribute (Name : String) return String;
      procedure Set_Attribute (Name, Value : String);

      function Get_Tasks return Sancta.Tasks.Containers.Lists.List;
      procedure Set_Tasks (Tasks : Sancta.Tasks.Containers.Lists.List);
      --  Tasks in its TO DO list.

   private

      Attributes : Agpl.Containers.String_String_Maps.Map;
      Tasks      : Sancta.Tasks.Containers.Lists.List;

   end Object;

   type Object_Access is access all Object;

   -------------------
   -- NETWORKED GUI --
   -------------------
   function Get_Next_Sequence return Types.Counter32;
   --  Note that this is not thread safe for now.

   type Network_Update_Kinds is (None,
                                 Pose, Status, Current_Task, All_Tasks, Trace);
   --  None is used to force updates in the visor

   type Network_Update (Kind : Network_Update_Kinds) is
     new Network.Message with
   record
      --  Sequence : Types.Counter32 := Get_Next_Sequence; -- To detect lost packets.
         case Kind is
            when None => null;
            when Pose =>
               Position : Types.Pose;
               Velocity : Types.Pose;
            when Status =>
               Info     : Ustring;
            when Current_Task =>
               Job      : Sancta.Tasks.Handle.Object;
            when All_Tasks =>
               Tasks    : Sancta.Tasks.Containers.Lists.List;
               Cost     : Sancta.Costs;
            when Trace =>
               Text     : Ustring;
               Level    : Agpl.Trace.Levels;
               Section  : Ustring;
         end case;
   end record;

end Sancta.Gui.Robot_Data;
