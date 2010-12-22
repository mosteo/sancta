with Sancta.Distributed.Datastore;

package Sancta.Distributed.Aliveness is

   --  Pragma preelaborate;

   procedure Update (Db : in out Datastore.Object'Class;
                     Ag : in String);
   --  Set this agent alive timestamp

   function Lateness (Db : not null access Datastore.Object'Class;
                      Ag : in String) return Duration;
   --  Say latest time this agent seen

end Sancta.Distributed.Aliveness;
