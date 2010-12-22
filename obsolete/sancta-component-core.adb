with Sancta.Datastore;
with Sancta.Component.Factory;
with Sancta.Component.Player_Safe;

with Agpl.Trace; use Agpl.Trace;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Core is

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      This : constant Object_Access := new Object;
      use Agpl.Xml;
   begin
      This.Prepare (Config);

      Player_Safe.Object.Connect
        (Get_Attribute (Config, "address", "127.0.0.1"),
         Positive'Value (Get_Attribute (Config, "port", "0")));

      Player_Safe.Object.Connect_Position2d_0;

      This.Reader.Start;

      Datastore.Object.Put (This.Key (Data_Is_Connected),
                            Datastore.Bool'(Value => True));

      return Component.Object_Access (This);
   end Create;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Object) is
   begin
      This.Reader.Stop;
      Player_Safe.Object.Disconnect;
   end Stop;

   ---------------
   -- Poll_Task --
   ---------------

   task body Poll_Task is
      use Ada.Calendar;

      Next    : Time              := Clock;
      Period  : constant Duration := 0.001;
      Done    : Boolean           := False;
   begin
      select
         accept Start;
      or
         accept Stop do
            Done := True;
         end Stop;
      end select;

      while not Done loop
         begin
            select
               delay 2.0;
               Log ("Core: Aborted Player polling", Warning);
               Done := True;
            then abort
               Player_Safe.Object.Poll_Player;
            end select;
         exception
            when E : others =>
               Log ("Core [polling]: " & Report (E), Error);
               Done := True;
         end;

         --  Wait a bit:
         Next := Next + Period;
         select
            accept Stop do
               Done := True;
            end Stop;
         or
            delay until Next;
         end select;
      end loop;

      Datastore.Object.Set (String (Data_Is_Connected),
                            Datastore.Bool'(Value => False));
   exception
      when E : others =>
         Log ("Core: Player Poll task terminated by unexpected exception: " &
              Report (E), Error);
         Datastore.Object.Put (Key (Parent.all, Data_Is_Connected),
                               Datastore.Bool'(Value => False));
   end Poll_Task;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Core;
