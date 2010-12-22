with Sancta.Debug2,
     Sancta.Tasks.Positioned,
     Sancta.Types.Operations;

package body Sancta.Agent_Proxy is

   use type Types.Real;

   ---------------
   -- Get_Alive --
   ---------------

   function Get_Alive (This : in Object) return Boolean is
   begin
      return This.Alive;
   end Get_Alive;

   -------------------
   -- Get_Last_Seen --
   -------------------

   function Get_Last_Seen (This : in Object) return Time is
   begin
      return This.Last_Seen;
   end Get_Last_Seen;

   ---------------
   -- Set_Alive --
   ---------------

   procedure Set_Alive (This : in out Object; Alive : in Boolean := True) is
   begin
      This.Alive     := Alive;
      This.Last_Seen := Clock;
   end Set_Alive;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This     : in out Object;
      The_Task : in out Sancta.Tasks.Primitive.Object'Class;
      Plan     : in out Sancta.Plan.Object;
      Done     :    out Boolean)
   is
      pragma Unreferenced (This, The_Task, Plan);
   begin
      Done := False;
   end Execute;

   ------------------------
   -- Set_Cost_Generator --
   ------------------------

   procedure Set_Cost_Generator
     (This : in out Object;
      Gen  :        Cost_Utils.Cost_Generator'Class)
   is
   begin
      This.Cost_Gen.Set (Gen);
   end Set_Cost_Generator;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This : in Object;
      From, To : in Sancta.Tasks.Object'Class)
      return Sancta.Costs
   is
   begin
      if This.Cost_Gen.Is_Valid then
         return This.Cost_Gen.Get.Get_Cost (From, To);
      else
         return Sancta.Cost_Utils.Get_Execution_Cost
           (From, To,
            Get_Pose (This),
            This.Lin_Speed,
            This.Ang_Speed);
      end if;
   end Get_Cost;

   --------------
   -- Get_Pose --
   --------------

   function Get_Pose (This : in Object) return Types.Pose is
   begin
      return This.Pose;
   end Get_Pose;

   --------------
   -- Get_Velo --
   --------------

   function Get_Velo (This : in Object) return Types.Pose is
   begin
      return This.Velo;
   end Get_Velo;

   -------------------
   -- Set_Ang_Speed --
   -------------------

   procedure Set_Ang_Speed (This  : in out Object;
                            Speed : in     Types.Real)
   is
   begin
      This.Ang_Speed := Speed;
   end Set_Ang_Speed;

   -------------------
   -- Set_Lin_Speed --
   -------------------

   procedure Set_Lin_Speed (This  : in out Object;
                            Speed : in     Types.Real)
   is
   begin
      This.Lin_Speed := Speed;
   end Set_Lin_Speed;

   --------------
   -- Set_Pose --
   --------------

   procedure Set_Pose
     (This : in out Object;
      Pose : in     Types.Pose)
   is
   begin
      This.Pose := Pose;
      if This.Cost_Gen.Is_Valid then
         This.Cost_Gen.Ref.Set_Pose (Pose);
      end if;
   end Set_Pose;

   --------------
   -- Set_Velo --
   --------------

   procedure Set_Velo (This : in out Object; Velo : in Types.Pose) is
   begin
      This.Velo := Velo;
   end Set_Velo;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (This : in Object) return Node_Id is
   begin
      return This.Id;
   end Get_Id;

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id (This : in out Object; Id : in Node_Id) is
   begin
      This.Id := Id;
   end Set_Id;

      --------------
   -- Finished --
   --------------

   function Finished (This : Object;
                      Job  : Tasks.Object'Class) return Boolean
   is
      use Types.Operations;
   begin
      if Job in Tasks.Positioned.Object'Class then
         if Distance (This.Get_Pose,
                      Tasks.Positioned.Object (Job).Pose) <= 1.2
         then
            return True;
         end if;
      end if;
      return False;
   end Finished;

   -----------
   -- Print --
   -----------

   procedure Print (This : Object) is
   begin
      Log ("AGENT_PROXY: " & This.Get_Name & " p: " &
           Debug2.To_String (This.Pose) & "; v: " &
           Debug2.To_String (This.Velo), Always, Log_Section);
   end Print;

end Sancta.Agent_Proxy;
