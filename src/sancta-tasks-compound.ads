 

--  Tasks of the network.

--  Compound specialization.

package Sancta.Tasks.Compound is

   pragma Preelaborate;

   type Object is abstract new Tasks.Object with null record;
   type Object_Access is access Object'Class;

   function Is_Primitive (This : in Object) return Boolean;
   --  Returns false.

end Sancta.Tasks.Compound;
