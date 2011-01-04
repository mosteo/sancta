with Sancta.Ctree.Data_Server;

package Sancta.Ctree.Message is

   --  pragma Preelaborate;

   type Kinds is (Shutdown, Use_Goals, Goal_Reached);

   type Object (Kind : Kinds; Length : Natural) is record
      case Kind is
         when Use_Goals =>
            Goals : Data_Server.Goal_Array (1 .. Length);
         when Goal_Reached | Shutdown =>
            null;
      end case;
   end record;

   function To_String (This : Object) return String;
   function Parse     (S    : String) return Object;

end Sancta.Ctree.Message;
