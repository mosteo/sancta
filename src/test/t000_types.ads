with Ada.Streams,
     Sancta.Network;

use Ada.Streams,
    Sancta;

package T000_Types is

   type Filler (Last : Stream_Element_Offset) is new Network.Message with record
      Dummy : Stream_Element_Array (1 .. Last);
   end record;

   for Filler'External_Tag use "f";

   type Check_Message (Length : Stream_Element_Offset) is new Network.Message
   with record
      Data : Stream_Element_Array (1 .. Length);
   end record;

   for Check_Message'External_Tag use "cm";

   not overriding
   procedure Selfcheck (M : Check_Message);

   not overriding
   function Create (Min_Size : Positive := 1;
                    Max_Size : Positive := 255) return Check_Message;

end T000_Types;
