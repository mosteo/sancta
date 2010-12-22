pragma Warnings (Off);
with Sancta.Agent.Containers;
with Sancta.Agent.Utils;
with Sancta.Network.Handles;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Utils;
with Sancta.Types;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

package Sancta.Containers is

   pragma Preelaborate;

   package Id_Vectors is
      new Ada.Containers.Vectors (Positive, Node_Id);

   package Agent_Containers renames Sancta.Agent.Containers;
   package Ac               renames Agent_Containers;
   package Agent_Lists   renames Agent.Containers.Lists;
   package Agent_Maps    renames Agent.Containers.Maps;
   package Agent_Vectors renames Agent.Containers.Vectors;
   package Agent_Utils   renames Agent.Utils;

   package Task_Containers renames Sancta.Tasks.Containers;
   package Tc              renames Task_Containers;
   package Task_Lists   renames Sancta.Tasks.Containers.Lists;
   package Task_Maps    renames Sancta.Tasks.Containers.Maps;
   package Task_Vectors renames Sancta.Tasks.Containers.Vectors;
   package Task_Utils   renames Sancta.Tasks.Utils;

   package Pose_Vectors renames Types.Pose_Vector;
   package Pv           renames Pose_Vectors;

   package String_Cost_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Costs);

   type Msg_Bundle is record
      Addr : Network.Handles.Address_Handle;
      Msg  : Network.Message_Handle;
   end record;

   function Bundle (Addr : Network.Address;
                    Msg  : Network.Message'Class) return Msg_Bundle;

   package Msg_Bundle_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Msg_Bundle);

end Sancta.Containers;
