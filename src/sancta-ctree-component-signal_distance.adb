with Agpl.Random;
with Sancta.Ctree.NCTypes;
with Sancta.Ctree.Single_Distributed;
with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Component.Helper;
with Sancta.Component.Network;
with Sancta.Network.Qualities;
with Sancta.Types.Operations;

package body Sancta.Ctree.Component.Signal_Distance is

   package SNC renames Sancta.Network.Qualities;

   type Object_Access is access all Object;

   procedure Update_Biases (This : in out Object) is
      procedure Update (I : Bias_Maps.Cursor) is
         Bias : Float := Float (Bias_Maps.Element (I));
      begin
         Bias := Bias + Agpl.Random.Get_Float (-This.Bias_Delta, This.Bias_Delta);
         Bias := Float'Max (Bias, -This.Bias_Amp);
         Bias := Float'Min (Bias, This.Bias_Amp);
         This.Biases.Include (Bias_Maps.Key (I), Bias);
      end Update;
   begin
      This.Biases.Iterate (Update'Access);
   end Update_Biases;

   -------------
   -- Noisify --
   -------------

   function Noisify (This : Object; L, R : Node_Id; S : Signal_Q) return Signal_Q
   is
      Q : Float := Float (S) + This.Biases.Element (Value (L, R));
   begin
      Q := Float'Max (0.0, Q);
      Q := Float'Min (Float (Signal_Q'Last), Q);
      return Signal_Q (Q);
   end Noisify;

   function Noisify (This : Object; Links : Nctypes.Full_Signal)
                     return Nctypes.Full_Signal
   is
      Result : Nctypes.Full_Signal := Links;
      Nodes  : constant SNC.Node_Vector := Links.Links.Nodes;
   begin
      for I in Nodes.First_Index .. Nodes.Last_Index loop
         for J in Nodes.First_Index .. Nodes.Last_Index loop
            if I < J or else
              (Links.Links.Directed_Source and Links.Links.Directed_Query)
            then
               declare
                  L : constant Node_Id := Value (Nodes.Element (I));
                  R : constant Node_Id := Value (Nodes.Element (J));
               begin
                  Result.Links.Set
                    (L, R, Noisify (This, L, R, Links.Links.Get (L, R)));
               end;
            end if;
         end loop;
      end loop;

      return Result;
   end Noisify;


   function Noisify (This  : Object;
                     Links : Ctree.Id_Q_Maps.Map) return Ctree.Id_Q_Maps.Map is
      Result : Ctree.Id_Q_Maps.Map;

      procedure Inner (I : Ctree.Id_Q_Maps.Cursor) is
         Other : constant Node_Id := Ctree.Id_Q_Maps.Key (I);
      begin
         Result.Insert
           (Other,
            Noisify (This, This.Id, Other, Ctree.Id_Q_Maps.Element (I)));
      end Inner;

   begin
      Links.Iterate (Inner'Access);
      return Result;
   end Noisify;

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

      This.Noise_Amp := Float'Value
        (This.Option (Opt_Random_Noise_Amplitude, Def_Random_Noise_Amplitude'Img));

      This.Bias_Amp := Float'Value
        (This.Option (Opt_Bias_Amplitude, Def_Bias_Amplitude'Img));

      This.Bias_Delta := Float'Value
        (This.Option (Opt_Bias_Amplitude, Def_Bias_Amplitude'Img)) * 2.0 /
             Float'Value (This.Option (Opt_Bias_Period, Def_Bias_Period'Img)) *
           Float (This.Period.Get_Period);

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

      Update_Biases (This);

      This.Output (Provides_Signal,
                   Nctypes.Signal'(Links => Noisify (This, This.Links)));

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
      This.Output (Provides_Full_Signal, Noisify (This, Full_Links));
   end Output_Full_Links;

end Sancta.Ctree.Component.Signal_Distance;
