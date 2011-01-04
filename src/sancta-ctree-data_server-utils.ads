with Sancta.Assignment;

package Sancta.Ctree.Data_Server.Utils is

   --  pragma Preelaborate;

   procedure Get_Poses (Bots :        Natural;
                        Ass  : in out Sancta.Assignment.Object);
   --  Fill-in this assignment with proxy agents having pose
   --  Robots marked as static in options are not added
   --  Any assigned tasks are kept

   procedure Set_Goals (Bots : Natural;
                        Ass  : Sancta.Assignment.Object);
   --  Robot tasks should be obviously of the Positioned class

end Sancta.Ctree.Data_Server.Utils;

