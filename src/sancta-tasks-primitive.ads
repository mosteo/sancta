 

--  Tasks of the network.

--  Primitive specialization.

package Sancta.Tasks.Primitive is

   pragma Preelaborate;

   type Object is abstract new Tasks.Object with null record;
   type Object_Access is access Object'Class;

   function Is_Primitive (This : in Object) return Boolean;
   --  Returns true.

end Sancta.Tasks.Primitive;
