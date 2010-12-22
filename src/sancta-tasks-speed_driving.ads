with Sancta.Types;

with Agpl.Chronos;
with Sancta.Tasks.Primitive;

package Sancta.Tasks.Speed_Driving is

--   pragma Elaborate_Body;

   type Object is new Sancta.Tasks.Primitive.Object with private;

   function Create (Velocity : in Types.Pose := Types.Null_Pose;
                    During   : in Duration   := 1.0) return Object;
   --  Creation

   function Elapsed (This : in Object) return Duration;
   --  Pre: Mark_Started called!!

   function Finished (This : in Object) return Boolean;
   --  Has been run for enough time?

   function Is_Started (This : in Object) return Boolean;

   function Lasts (This : in Object) return Duration;
   --  The original During parameter in creation

   function Remains (This            : in Object;
                     Always_Positive : in Boolean := True)
                     return               Duration;
   --  Says time remaining. If Always_Positive, negative is returned as 0.0

   procedure Mark_Started (This : in out Object);
   --  For executers, notify that we're starting the task now

   function Velocity (This : in Object) return Types.Pose;

private

   type Object is new Sancta.Tasks.Primitive.Object with
      record
         Velocity : Types.Pose := Types.Null_Pose;
         Started  : Boolean    := False;
         Since    : Agpl.Chronos.Object;
         During   : Duration   := 1.0;
      end record;

end Sancta.Tasks.Speed_Driving;
