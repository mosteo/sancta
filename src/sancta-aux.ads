with Sancta.Distributed;

--  This package just contains miscelaneous trash for testcases and bugcases.

package Sancta.Aux is

   type Int is new Distributed.Object_Data with record
      I : Integer;
   end record;

end Sancta.Aux;
