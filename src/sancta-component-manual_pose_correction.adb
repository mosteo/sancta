with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Network.Messages;
with Sancta.Component.Factory,
     Sancta.Component.Network,
     Sancta.Component.Types;

--  with Agpl.Monitor;
with Agpl; use Agpl;

--  with Ada.Text_Io; use Ada.Text_Io;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Manual_Pose_Correction is

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Verify (Option_Channel);
      This.Link :=
        Sancta.Network.Layer.Object_Access
          (Component.Network.Network (This.Input (Requires_Link)).Link);
      This.Link.Subscribe
        (This.Listener'Access,
         Sancta.Network.Value (This.Option (Option_Channel)));

      This.Output (Provides_Out_Pose,
                   Types.Robot_Pose'(Pose => Sancta.Types.Origin));

      This.Subscribe (Requires_In_Pose);
      This.Subscribe (Requires_New_Pose);
      This.Subscribe (Requires_Inverse_Pose);

      return Component.Object_Access (This);
   end Create;

   ----------------------
   -- Process_New_Pose --
   ----------------------

   procedure Process_New_Pose (This : in out Object; Pose : Types.Pose) is
      use Sancta.Types;
      use Sancta.Types.Transformations;
   begin
      Log ("Received pose correction: " &
           To_String (Pose.Pose),
           Debug, Section => Log_Section);

      if This.Provided (Requires_In_Pose) then
         This.Transf :=
           Get_Composition   (+Pose.Pose) *
           Get_Decomposition (+Types.Pose (This.Input (Requires_In_Pose)).Pose);
         This.Inv_Transf := Transformations.Real_Transf.Inverse (This.Transf);

         Log ("Pose adjusted from " &
              To_STring (Types.Pose (This.Input (Requires_In_Pose)).Pose) &
              " to " & To_String (Pose.Pose),
              Debug, Section => Log_Section);
      end if;

      This.Output (Provides_Out_Pose,
                   Types.Robot_Pose'(Pose => Pose.Pose));
   end Process_New_Pose;

   -------------------
   -- On_Key_Stored --
   -------------------

   --  Hack for mutual exclusion:
--     Mutex : aliased Monitor.Semaphore;
   --  I don't think this is necessary, since there's no Run and thus no
   --  possibility of concurrency problems from Key_Stored

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      use Sancta.Types;
      use Sancta.Types.Transformations;
--      M : Monitor.Object (Mutex'Access); pragma Unreferenced (M);
   begin
      if Key = Requires_In_Pose then
         This.Output
           (Provides_Out_Pose,
            Types.Robot_Pose'
              (Pose => +(This.Transf * (+Types.Pose (Value).Pose))));
      elsif Key = Requires_New_Pose then
         Process_New_Pose (This, Types.Pose (Value));
      elsif Key = Requires_Inverse_Pose then
         Log ("Received inverse pose: " & To_String (Types.Pose (Value).Pose),
              Debug, Log_Section);
         This.Output
           (Provides_Out_Inverse_Pose,
            Types.Robot_Pose'
              (Pose => +(This.Inv_Transf * (+Types.Pose (Value).Pose))));
         Log
           ("Pose inversed from " & To_STring (Types.Pose (Value).Pose) &
            " to " &
            To_String (+(This.Inv_Transf * (+Types.Pose (Value).Pose))),
            Debug, Section => Log_Section);
      else
         raise Program_Error with "Unknown key: " & String (Key);
      end if;
   end Key_Stored;

   ------------------
   -- On_Reception --
   ------------------

   procedure On_Reception (Thix : in out Listener_Type;
                           M    : in     Sancta.Network.Message'Class;
                           Meta : in     Sancta.Network.Message_Metadata)
   is
      pragma Unreferenced (Meta);
      This : access Object renames Thix.This;
   begin
      if M in Sancta.Network.Messages.Set_Pose_Type then
         --  Store the given pose, this will trigger the new transform.
         Log ("Received new pose message, storing...", Debug, Log_Section);
         This.Output
           (Provides_Manual_Pose,
            Types.Robot_Pose'
              (Pose => Sancta.Network.Messages.Set_Pose_Type (M).Pose));

         Process_New_Pose
           (This.all,
            Types.Pose'
              (Pose => Sancta.Network.Messages.Set_Pose_Type (M).Pose));
      end if;
   end On_Reception;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Manual_Pose_Correction;
