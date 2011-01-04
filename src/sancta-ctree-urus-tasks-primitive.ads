with Sancta.Tasks.Positioned,
     Sancta.Tasks.Primitive;

package Sancta.Ctree.Urus.Tasks.Primitive is

   pragma Preelaborate;

   type Go_To (Use_Angle : Boolean) is
     new Sancta.Tasks.Positioned.Object with null record;

   type Approach_Requester
     is new Sancta.Tasks.Primitive.Object with null record;

   type Load
     is new Sancta.Tasks.Primitive.Object with null record;

   type Unload
     is new Sancta.Tasks.Primitive.Object with null record;

end Sancta.Ctree.Urus.Tasks.Primitive;
