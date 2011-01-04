with Sancta.Ctree.Connectivity_Matrix;

with Sancta.Assignment;

package Sancta.Ctree.Data_Source is

   pragma Preelaborate;

   type Object is abstract tagged null record;

   function Get_Links (This : Object;
                       Bots : Natural)
                       return Connectivity_Matrix.Object is abstract;

   function Get_Poses (This : Object;
                       Bots : Natural)
                       return Sancta.Assignment.Object is abstract;

   procedure Set_Goals (This : Object;
                        Bots : Natural;
                        Ass  : Sancta.Assignment.Object) is abstract;
   --  Local setting, for the local CNM

   procedure Send_Goals (This : Object;
                         Bots : Natural;
                         Ass  : Sancta.Assignment.Object) is abstract;
   --  Network sending for slaves

   function Receive_Goals (This : Object) return Sancta.Assignment.Object
   is abstract;
   --  Receive from network a list of goals that must be sent to CNM
   --  If no new orders, the previous ones should be returned cached,
   --  or the assignment should be empty.

   --  FACTORY

   procedure Register (Src : Data_Sources; This : Object'Class);

   function  Get (Src : Data_Sources) return Object'Class;

end Sancta.Ctree.Data_Source;
