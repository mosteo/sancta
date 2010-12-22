with Agpl.Drawing;
with Sancta.Agent,
     Sancta.Tasks.Interfaces,
     Sancta.Tasks.Primitive,
     Sancta.Types;

package Sancta.Tasks.Positioned is

   pragma Preelaborate;

   type Object is new Sancta.Tasks.Primitive.Object
     and Interfaces.Completable
     and Agpl.Drawing.Drawable
   with record
      Pose : Types.Pose;
   end record;
   --  Root task type used as ancestor of position-based tasks, to simplify
   --  later the computation of costs.

   function Create (Pose : Types.Pose) return Object'Class;

   not overriding
   function Get_Pose (This : Object) return Types.Pose;
   pragma Inline (Get_Pose);

   not overriding
   procedure Set_Pose (This : in out Object; Pose : Types.Pose);

   overriding
   function Completed (This : Object;
                       Bot  : Agent.Object'Class) return Boolean;

   overriding
   procedure Draw (This :        Object;
                   D    : in out Agpl.Drawing.Drawer'Class);

end Sancta.Tasks.Positioned;
