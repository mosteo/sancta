package Sancta.Ctree.Springs_C is

   pragma Preelaborate;

   type Params is record
      Num_Robots : Positive;
      K          : Float;
      Goal_V     : Float;
      Rest_Pos   : Float;
      Kv         : Float;
      F          : Float;
   end record;

   task type Springs (Config      : access Params);
   --  Create a new task of this to have the C springs working...

end Sancta.Ctree.Springs_C;
