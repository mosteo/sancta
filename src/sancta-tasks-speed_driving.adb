package body Sancta.Tasks.Speed_Driving is

   ------------
   -- Create --
   ------------

   function Create (Velocity : in Types.Pose := Types.Null_Pose;
                    During   : in Duration   := 1.0) return Object
   is
   begin
      return (Sancta.Tasks.Primitive.Object with
              Velocity => Velocity,
              Started  => False,
              During   => During,
              others   => <>);
   end Create;

   ----------------
   -- Is_Started --
   ----------------

   function Is_Started (This : in Object) return Boolean is
   begin
      return This.Started;
   end Is_Started;

   ------------------
   -- Mark_Started --
   ------------------

   procedure Mark_Started (This : in out Object) is
   begin
      This.Started := True;
      This.Since.Reset;
   end Mark_Started;

   -------------
   -- Elapsed --
   -------------

   function Elapsed (This : in Object) return Duration is
   begin
      if not This.Started then
         raise Constraint_Error;
      else
         return This.Since.Elapsed;
      end if;
   end Elapsed;

   --------------
   -- Finished --
   --------------

   function Finished (This : in Object) return Boolean is
   begin
      return This.Elapsed >= This.During;
   end Finished;

   -----------
   -- Lasts --
   -----------

   function Lasts (This : in Object) return Duration is
   begin
      return This.During;
   end Lasts;

   -------------
   -- Remains --
   -------------

   function Remains (This            : in Object;
                     Always_Positive : in Boolean := True)
                     return               Duration
   is
   begin
      if not This.Started then
         return This.Lasts;
      elsif Always_Positive then
         return Duration'Max (0.0, This.During - This.Elapsed);
      else
         return This.During - This.Elapsed;
      end if;
   end Remains;

   --------------
   -- Velocity --
   --------------

   function Velocity (This : in Object) return Types.Pose is
   begin
      return This.Velocity;
   end Velocity;

end Sancta.Tasks.Speed_Driving;
