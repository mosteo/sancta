with Sancta.Distributed.Types;
with Agpl.Calendar.Format;

package body Sancta.Distributed.Aliveness is

   ---------------
   -- Agent_Key --
   ---------------

   function Agent_Key (Ag : in String) return Object_Key is
   begin
      return + (Ag & ".alive");
   end Agent_Key;

   ------------
   -- Update --
   ------------

   procedure Update (Db : in out Datastore.Object'Class;
                     Ag : in String)
   is
      Stamp : Types.Dstring :=
                Types.Value (Agpl.Calendar.Format.Timestamp);
   begin
      Db.Set (Agent_Key (Ag), Stamp);
   end Update;

   --------------
   -- Lateness --
   --------------

   function Lateness (Db : not null access Datastore.Object'Class;
                      Ag : in String) return Duration
   is
      Stamp : Object_Data_Handle.Object;
      Meta  : Object_Metadata;
   begin
      Db.Get (Agent_Key (Ag), Stamp, Meta);
      if Stamp.Is_Valid then
         if Image (Meta.Owner) = Ag then
            return Meta.Last_Seen_Owner.Elapsed;
         else
            return Duration'Last;
         end if;
      else
         return Duration'Last;
      end if;
   end Lateness;

end Sancta.Distributed.Aliveness;
