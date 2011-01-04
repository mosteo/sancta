with Sancta.Ctree.NCTypes;
with Sancta.Ctree.Single_Distributed;
with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Component.Helper;
with Sancta.Component.Network;
with Sancta.Types.Operations;

package body Sancta.Ctree.Component.Ctree_Signal_Distance is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------

   procedure Process_Incoming_Packet
     (This : in out Listener_Type;
      M    : in     Sancta.Network.Message'Class;
      Meta : in     Sancta.Network.Message_Metadata)
   is
      pragma Unreferenced (Meta);
      use Sancta.Ctree.Single_Distributed;
   begin
      if M in Msg_Robot_Global_Update then
         declare
            Update : Msg_Robot_Global_Update renames Msg_Robot_Global_Update (M);
         begin
            This.Parent.Poses.Include (Update.Id, Update.Pose);

            if Update.Id = This.Parent.Id then
               This.Parent.Pose       := Update.Pose;
               This.Parent.Pose_Valid := True;
               --  ourselves updated, update link to base
               This.Parent.Add_Link
                 (Value       (This.Parent.Option (Opt_Base_Id)),
                  Ctypes.Pose (This.Parent.Input  (Requires_Base_Pose)).Pose);
            elsif This.Parent.Pose_Valid then
               --  update some neighbor
               This.Parent.Add_Link (Update.Id,
                                     Update.Pose);
            end if;
         end;
      end if;
   end Process_Incoming_Packet;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Comp_Config;
      Env    : Environment.Object)
      return Sancta.Component.Object_Access
   is
      This : constant Object_Access :=
               new Object
                 (Name'Access,
                  Config,
                  Sancta.Component.Network.Network
                    (Helper.Input (Config, Requires_Link)).Link);
   begin
      This.Period.Set_Period
        (Duration'Value (This.Option (Opt_Period, Def_Period'Img)));

      This.Listener.Subscribe
        (Sancta.Network.Value (This.Option (Opt_Channel)));

      This.Id        := Env.Id;
      This.Drop_Dist := Types.Real'Value (This.Option (Opt_Drop_Dist));

      return Sancta.Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
   begin
      This.Listener.Run;
      This.Output (Provides_Signal,
                   Nctypes.Signal'(Links => This.Links));

      This.Output_Full_Links;

      This.Period.Next (Next);
   end Run;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link (This     : in out Object;
                       Other_Id :        Node_Id;
                       Other_P  :        Types.Pose)
   is
      use Sancta.Types.Operations;
      use type Types.Real;
   begin
      Log ("Updating link " & Image (This.Id) & " -- " & image (other_id),
           Debug, Log_Section);
      This.Links.Include
        (Other_Id,
         Signal_Q
           (Types.Real'Max
              (0.0,
               100.0 *
                 (This.Drop_Dist - Distance (This.Pose, Other_P)) /
                 This.Drop_Dist)));
   end Add_Link;

   -----------------------
   -- Output_Full_Links --
   -----------------------

   procedure Output_Full_Links (This : in out Object) is
      Full_Links : Nctypes.Full_Signal (Directed_Source   => False,
                                        Directed_Query    => False,
                                        Missing_As_Broken => True);

      use Id_Pose_Maps;
      use Sancta.Types;
      use Sancta.Types.Operations;

      procedure Outer_Loop (I : Cursor) is
         procedure Inner_Loop (J : Cursor) is
         begin
            Full_Links.Links.Set
              (Key (I), Key (J),
               Signal_Q
                 (Types.Real'Max
                    (0.0,
                     100.0 *
                       (This.Drop_Dist - Distance (Element (I), Element (J))) /
                       This.Drop_Dist)));
         end Inner_Loop;
      begin
         This.Poses.Iterate (Inner_Loop'Access);
      end Outer_Loop;
   begin
      This.Poses.Iterate (Outer_Loop'Access);
      This.Output (Provides_Full_Signal, Full_Links);
   end Output_Full_Links;

end Sancta.Ctree.Component.Ctree_Signal_Distance;
