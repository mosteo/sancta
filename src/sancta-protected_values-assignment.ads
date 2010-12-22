 

--  Protected container for values of any private type.

with Agpl.Protected_Value;
with Sancta.Assignment;

package Sancta.Protected_Values.Assignment is
  new Agpl.Protected_Value (Sancta.Assignment.Object);

pragma Preelaborate (Sancta.Protected_Values.Assignment);
