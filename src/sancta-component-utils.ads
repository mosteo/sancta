with Sancta.Component.Root,
     Sancta.Types;
pragma Elaborate_All (Sancta.Component.Root);

package Sancta.Component.Utils is

   function Option is new
     Root.Get_Option (Integer, Integer'Image, Integer'Value);

   function Option is new
     Root.Get_Option (Float, Float'Image, Float'Value);

   function Option is new
     Root.Get_Option (Types.Real, Types.Real'Image, Types.Real'Value);

   function Option is new
     Root.Get_Option (Boolean, Boolean'Image, Boolean'Value);

   function Option is new
     Root.Get_Option (Duration, Duration'Image, Duration'Value);

   function Option is new
     Root.Get_Option (Costs, Costs'Image, Costs'Value);

end Sancta.Component.Utils;
