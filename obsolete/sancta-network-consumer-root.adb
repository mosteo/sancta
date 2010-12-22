with Sancta.Network.Groups;

package body Sancta.Network.Consumer.Root is

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe
     (This     : in out Object;
      Kind     : in     Tag)
   is
   begin
      This.Link.Subscribe (This'Access, Kind);
   end Subscribe;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe
     (This     : in out Object;
      Kinds    : in     Network.Tag_Array)
   is
   begin
      for I in Kinds'Range loop
         This.Subscribe (Kinds (I));
      end loop;
   end Subscribe;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe
     (This     : in out Object;
      Channel  : in     Network.Group_Id)
   is
   begin
      Subscribe (This, Network.Groups.Channel_Tags (Channel));
   end Subscribe;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (This     : in Consumer.Object_Access;
                        Link     : not null access Layer.Object'Class;
                        Kind     : in     Tag)
   is
   begin
      Link.Subscribe (This, Kind);
   end Subscribe;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (This     : in Consumer.Object_Access;
                        Link     : not null access Layer.Object'Class;
                        Kinds    : in     Network.Tag_Array)
   is
   begin
      Link.Subscribe (This, Kinds);
   end Subscribe;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (This     : in Consumer.Object_Access;
                        Link     : not null access Layer.Object'Class;
                        Channel  : in     Network.Group_Id)
   is
   begin
      Link.Subscribe (This, Network.Groups.Channel_Tags (Channel));
   end Subscribe;

end Sancta.Network.Consumer.Root;
