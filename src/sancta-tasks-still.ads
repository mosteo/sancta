 

--  This task is to be still for some period of time.

with Sancta.Tasks.Primitive;

with Ada.Calendar; use Ada.Calendar;

package Sancta.Tasks.Still is

   pragma Elaborate_Body;

   type Object (<>) is new Tasks.Primitive.Object with private;
   type Object_Access is access Object'Class;

   function Create (Period : in Duration;
                    Start  : in Time := Clock) return Object;

   function Is_Still (This    : in Object;
                      Instant : in Time := Clock) return Boolean;
   --  Says if the time is in the still period

private

   type Object is new Tasks.Primitive.Object with
      record
         Start  : Time;
         Period : Duration;
      end record;

end Sancta.Tasks.Still;
