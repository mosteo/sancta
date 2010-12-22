with Ada.Numerics.Generic_Elementary_Functions;

package Sancta.Types.Real_Math is new
Ada.Numerics.Generic_Elementary_Functions (Real);

pragma Preelaborate (Sancta.Types.Real_Math);
