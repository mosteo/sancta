--  with Sancta.Debug2;

package body Sancta.Network.Messages is

   --------------
   -- Set_Pose --
   --------------

   function Set_Pose (Pose : in Types.Pose) return Set_Pose_Type is
   begin
      return (Pose => Pose);
   end Set_Pose;

   ------------------
   -- Propose_Task --
   ------------------

   function Propose_Task
     (This : in Sancta.Tasks.Object'Class)
      return Propose_Task_Type
   is
   begin
      return (Job => Sancta.Tasks.Handle.Set (This));
   end Propose_Task;

   ---------------
   -- Set_Tasks --
   ---------------

   function Set_Tasks (This : in Sancta.Tasks.Containers.Lists.List) return Set_Tasks_Type is
   begin
      return (Jobs => This);
   end Set_Tasks;

   function Set_Task (This : in Sancta.Tasks.Object'Class) return Set_Tasks_Type is
      L : Sancta.Tasks.Containers.Lists.List;
   begin
      L.Append (This);
      return (Jobs => L);
   end Set_Task;

   ---------------
   -- Task_Done --
   ---------------

   function Task_Done (This : in Sancta.Tasks.Task_Id) return Task_Done_Type is
   begin
      return (Id => This);
   end Task_Done;

   -----------
   -- Laser --
   -----------

   function Laser (Scan : Component.Types.Range_Scan) return Laser_Type
   is
      Msg  : Laser_Type (Last => Scan.Scan.Ref.Last);
      I    : Integer := Scan.Scan.Ref.Ranges'First;
   begin
      Msg.Robot_Pose := Scan.Robot_External_Pose;
      Msg.Laser_Pose := Types.Pose (Scan.Sensor_Pose_On_Robot);

--        Log ("rp: " & Debug2.To_String (Msg.Robot_Pose), always);
--        Log ("lp: " & Debug2.To_String (Msg.Laser_Pose), always);

      for J in 1 .. Msg.Last loop
         Msg.Range_Scan (J).A := Scan.Scan.Ref.Ranges (I).A;
         Msg.Range_Scan (J).D := Scan.Scan.Ref.Ranges (I).D;
         I := I + 1;
      end loop;
      return Msg;
   end Laser;

   --------------
   -- Redirect --
   --------------

   function Redirect (Key : Agpl.Protected_Datastore.Object_Key;
                      Val : Agpl.Protected_Datastore.Object_Data'Class;
                      ACK : Boolean := False)
                      return Redirect_Type
   is
      use Datastore_Key_Handle;
      use Datastore_Object_Handle;
   begin
      return (Key => +Key,
              Val => +Val,
              Ack => ACK);
   end Redirect;

end Sancta.Network.Messages;
