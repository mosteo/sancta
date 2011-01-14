with Ada.Containers.Ordered_Maps;
with Ada.Text_IO; use Ada.Text_IO;
with Agpl.Strings; use Agpl.Strings;
with Sancta.Ctree.Nctypes;
with Sancta.Ctree.Single_Distributed;
with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Component.Helper;
with Sancta.Component.Network;
with Sancta.Types.Operations;

package body Sancta.Ctree.Component.Signal_Tunnel is

   package Qmaps is new Ada.Containers.Ordered_Maps
     (Float, Signal_Q);

   Qmap : Qmaps.Map;

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
      use Sancta.Ctree.Single_Distributed;
   begin
      if M in Msg_Robot_Local_Update then
         declare
            Update : Msg_Robot_Local_Update renames Msg_Robot_Local_Update (M);
         begin
            if Meta.Sender = This.Parent.Id then
               This.Parent.Pose       := Update.Pose;
               This.Parent.Pose_Valid := True;
               --  ourselves updated, update link to base
               This.Parent.Add_Link
                 (Value       (This.Parent.Option (Opt_Base_Id)),
                  Ctypes.Pose (This.Parent.Input  (Requires_Base_Pose)).Pose);
            elsif This.Parent.Pose_Valid then
               --  update some neighbor
               This.Parent.Add_Link (Meta.Sender,
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
      This.Listener.Subscribe
        (Sancta.Network.Value (This.Option (Opt_Channel)));

      This.Id        := Env.Id;

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
      This.Period.Next (Next);
      This.Listener.Run;
      This.Output (Provides_Signal,
                   Nctypes.Signal'(Links => This.Links));
   end Run;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link (This     : in out Object;
                       Other_Id :        Node_Id;
                       Other_P  :        Types.Pose)
   is
      use Sancta.Types.Operations;
   begin
      Log ("Updating link " & Image (This.Id) & " -- " & image (other_id),
           Debug, Log_Section);

      This.Links.Include (Other_Id, Q (Float (Distance (This.Pose, Other_P))));
   end Add_Link;

   -------
   -- Q --
   -------

   function Q (Dist : Float) return Signal_Q is
   begin
      if Dist > Tunel_Data (Tunel_Data'Last).Dist then
         return Signal_Q'First;
      elsif Dist < Tunel_Data (Tunel_Data'First).Dist then
         return Signal_Q'Pred (Signal_Q'Last);
      else
         declare
            I : Qmaps.Cursor := Qmap.Floor (Dist);
            Table_Dist : Float    := Float'Last;
            RSSI       : Signal_Q;

            use Qmaps;
         begin
            for Try in 1 .. 2 loop
               if Has_Element (I) then
                  if abs (Key (I) - Dist) < Table_Dist then
                     Table_Dist := abs (Key (I) - Dist);
                     RSSI       := Element (I);
                  end if;
               else
                  exit;
               end if;
               Next (I);
            end loop;
            if Table_Dist < Float'Last then
               return RSSI;
            else
               raise Constraint_Error;
            end if;
         end;
      end if;
   end Q;

   -------------------
   -- Fading_Report --
   -------------------

   procedure Fading_Report is
      Fading_Start : Positive := Tunel_Data'First;
   begin
      --  Report fading sizes:
      for I in Tunel_Data'Range loop
         if I > Tunel_Data'First then
            if Tunel_Data (I - 1).RSSI > 0.0 and then Tunel_Data (I).RSSI = 0.0 then
               Fading_Start := I;
            end if;

            if Tunel_Data (I).RSSI = 0.0 and then Tunel_Data (I + 1).RSSI > 0.0 then
               Put_Line
                 ("Fading from " &
                  Rpad (To_String (Tunel_Data (Fading_Start).Dist, 3), 7) & " to " &
                  Rpad (To_String (Tunel_Data (I).Dist, 3), 7) & ", " &
                  "size (]: " & Rpad (To_String (Tunel_Data (I + 1).Dist - Tunel_Data (Fading_Start).Dist, 3), 5) &
                  " [): " & Rpad (To_String (Tunel_Data (I).Dist - Tunel_Data (Fading_Start - 1).Dist, 3), 5) &
                  " []: " & Rpad (To_String (Tunel_Data (I + 1).Dist - Tunel_Data (Fading_Start - 1).Dist, 3), 5) &
                  " (): " & Rpad (To_String (Tunel_Data (I).Dist - Tunel_Data (Fading_Start).Dist, 3), 5));
            end if;
         end if;
      end loop;
   end Fading_Report;

begin
   for I in Tunel_Data'Range loop
      Qmap.Include (Tunel_Data (I).Dist, Tunel_Data (I).RSSI);
   end loop;

   Fading_Report;

--     for I in 0 .. 35000 loop
--        Put_Line (To_String (Float (I) / 100.0, 3) & " " &
--                  To_String (Float (Q (Float (I) / 100.0)), 3));
--     end loop;
end Sancta.Ctree.Component.Signal_Tunnel;
