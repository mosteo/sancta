package Sancta.Ctree.Data_Source.Integrated is

   type Object is new Data_Source.Object with null record;

   function Get_Links (This : Object;
                       Bots : Natural)
                       return Connectivity_Matrix.Object;

   function Get_Poses (This : Object;
                       Bots : Natural)
                       return Sancta.Assignment.Object;

   procedure Set_Goals (This : Object;
                        Bots : Natural;
                        Ass  : Sancta.Assignment.Object);

   procedure Send_Goals (This : Object;
                         Bots : Natural;
                         Ass  : Sancta.Assignment.Object) is null;

   function Receive_Goals (This : Object) return Sancta.Assignment.Object;

end Sancta.Ctree.Data_Source.Integrated;
