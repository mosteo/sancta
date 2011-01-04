with Sancta.Tasks.Compound;

package Sancta.Ctree.Urus.Tasks.Compound is

   --  Called compound instead of Abstract because the latter is reserved in Ada

   type Relations is (Execute_All,
                      Execute_One,
                      Execute_Sequentially,
                      Execute_Concurrently);

   --  Missions/submissions

   type Mission is interface;

   type Urus_Compound (Relation : Relations) is
     abstract new Sancta.Tasks.Compound.Object with null record;

   not overriding
   function Is_Auctionable (This : Urus_Compound) return Boolean is abstract;

   --  Transportation/TAXI

   type Transportation_Taxi is new Urus_Compound (Execute_Sequentially)
     and Mission with null record;

   overriding
   function Is_Auctionable (This : Transportation_Taxi) return Boolean;

end Sancta.Ctree.Urus.Tasks.Compound;
