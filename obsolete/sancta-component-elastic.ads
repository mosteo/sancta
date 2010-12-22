--  Sample empty component to save-as when creating new ones.

with Sancta.Distributed.Datastore;
with Sancta.Robot_Hard;
with Sancta.Component.Root;

package Sancta.Component.Elastic is

   Plugin_Name : constant String := "elastic";

   Requires_Agent    : constant Internal_Key := "agent";
   Requires_Database : constant Internal_Key := "database";

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   not overriding
   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   --  Any of the following subprograms can be removed entirely if they're not
   --  going to be used, since there's null defaults in the Root class.

   overriding
   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Agpl.Protected_Datastore.Object_Key;
      Value : in     Agpl.Protected_Datastore.Object_Data'Class) is null;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

--     overriding
--     procedure Stop (This : in out Object) is null;

private

   type Object is new Root.Object with record
      Next   : Ada.Calendar.Time;
      Period : Duration := 0.5;

      Acquire_Poses : Boolean;
      --  If true, get the poses/vels and compute the control law.
      --  It is stored also in the shared DB.

      Update_Pose   : Boolean;
      --  If true, update your node poses/vels in the shared DB.

      Bot           : access Robot_Hard.Object'Class;
      Sdb           : access Distributed.Datastore.Object'Class;
   end record;

end Sancta.Component.Elastic;
