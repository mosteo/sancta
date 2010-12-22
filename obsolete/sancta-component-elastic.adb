with Sancta.Convert;
with Sancta.Datastore;
with Sancta.Distributed;
with Sancta.Distributed.Types;
with Sancta.Component.Factory;
pragma Elaborate_All (Sancta.Component.Factory);
with Sancta.Component.Player_Safe;
with Sancta.Component.Shared_Database;

with Agpl.Trace; use Agpl.Trace;

with Player;
with Player.Aux;
with Player.Types;

with Ada.Containers.Indefinite_Ordered_Maps;
with Interfaces.C;

package body Sancta.Component.Elastic is

   use Ada.Calendar;

   type Position2d_Struct is record
      Reserved  : Player.Aux.Reserved_Array
        (1 .. Natural (Player.Aux.Get_Device_T_Size));

      Geom_Pose : Player.Pose;
      Geom_Size : Player.Double_Array (1 .. 2);

      Odom_Pose : Player.Pose;
      Velocity  : Player.Pose;

      Stall     : Standard.Interfaces.C.int;
   end record;
   pragma Convention (C, Position2d_Struct);

   package Position2d_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (String, Position2d_Struct);

   ---------------------
   -- Agent_Store_Key --
   ---------------------
   --  Key to store current bot state
   function Agent_Store_Key (This : String) return Distributed.Object_Key is
      use Distributed;
   begin
      return +("elastic.state." & This);
   end Agent_Store_Key;

   ----------------------
   -- Agent_Update_Key --
   ----------------------
   --  Key to store some bot control law
   function Agent_Update_Key (This : String) return Distributed.Object_Key is
      use Distributed;
   begin
      return +("elastic.control." & This);
   end Agent_Update_Key;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      use Agpl.Xml;
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);
      This.Next := Clock + This.Period;

      This.Acquire_Poses := Boolean'Value
        (Get_Attribute (Config, "acquire_poses", "false"));
      This.Update_Pose   := Boolean'Value
        (Get_Attribute (Config, "update_pose", "true"));

      This.Bot :=
        Robot_Hard.Object_Access
          (Datastore.Robot
               (Datastore.Object.Get
                    (This.Key (Requires_Agent))).Ref);
      This.Sdb := Shared_Database.Get;

      if This.Acquire_Poses then
         Player_Safe.Object.Connect_Graphics2d_0;
      end if;

      return Component.Object_Access (This);
   end Create;

   -----------------
   -- Update_Pose --
   -----------------

   procedure Do_Update_Pose (This : in out Object) is
      use Distributed.Types;
      Value : Dposition2d := (Position => This.Bot.Get_Pose,
                              Velocity => This.Bot.Get_Vel);
   begin
      --  Update our data
      This.Sdb.Set (Agent_Store_Key (This.Bot.Get_Name), Value);

      --  And apply the new velocity settings:
      if This.Sdb.Contains (Agent_Update_Key (This.Bot.Get_Name)) then
         declare
            New_Vel : constant Dposition2d :=
                        Dposition2d
                          (This.Sdb.Get (Agent_Update_Key (This.Bot.Get_Name)));
         begin
            This.Bot.Set_Vel (New_Vel.Velocity.X,
                              New_Vel.Velocity.Y,
                              Types.Real (New_Vel.Velocity.A));
         end;
      end if;
   end Do_Update_Pose;

   -------------------
   -- Acquire_Poses --
   -------------------

   procedure Do_Acquire_Poses (This : in out Object) is
      use Position2d_Maps;
      Inputs : Map;
   begin
      --  Harvest input data
      for I in Types.Agent_Names'Range loop
         if This.Sdb.Contains (Agent_Store_Key (Types.Agent_Names (I))) then
            declare
               Pos2d : constant Distributed.Types.Dposition2d :=
                         Distributed.Types.Dposition2d
                           (This.Sdb.Get
                              (Agent_Store_Key (Types.Agent_Names (I))));
               Pos2d_Struct : Position2d_Struct;
               use Convert;
            begin
               Pos2d_Struct.Odom_Pose := +Pos2d.Position;
               Pos2d_Struct.Velocity  := +Pos2d.Velocity;
               Inputs.Insert (Types.Agent_Names (I), Pos2d_Struct);
            end;
         end if;
      end loop;

      --  Compute springs
      declare
         type Position2d_Array is
           array (1 .. Interfaces.C.Int (Inputs.Length)) of Position2d_Struct;
         pragma Convention (C, Position2d_Array);

         C_Input  : Position2d_Array;
         C_Output : Position2d_Array;
         I        : Cursor := Inputs.First;
         J        : Interfaces.C.Int := C_Input'First;
         use type Interfaces.C.Int;

         procedure Compute_Springs (Input,
                                    Output : in out Position2d_Array;
                                    Num    :        Interfaces.C.Int);
         pragma Import (C, Compute_Springs, "compute_springs");
      begin
         while Has_Element (I) loop
            C_Input (J) := Element (I);
            J := J + 1;
            Next (I);
         end loop;

         Player_Safe.Object.Graphics_Clear;
         Player_Safe.Object.Graphics_Set_Color ((0, 0, 0, 255));
         Player_Safe.Object.Graphics_Draw_Polyline (((0.0, 0.0), (10.0, 10.0)));
         Player_Safe.Object.Graphics_Set_Color ((0, 255, 0, 0));

         for I in C_Input'First .. C_Input'Last - 1 loop
            for J in I + 1 .. C_Input'Last loop
               Player_Safe.Object.Graphics_Draw_Polyline
                 ((1 => (Player.Types.Player_Float (C_Input (I).Odom_Pose (1)),
                         Player.Types.Player_Float (C_Input (I).Odom_Pose (2))),
                   2 => (Player.Types.Player_Float (C_Input (J).Odom_Pose (1)),
                         Player.Types.Player_Float (C_Input (J).Odom_Pose (2)))));
               Log ("Drawing line" & I'Img, Always);
            end loop;
         end loop;

--           Compute_Springs (C_Input, C_Output, C_Input'Length);
--
--           --  Communicate control actions
--           declare
--              I : Cursor := Inputs.First;
--              J : Interfaces.C.Int := C_Output'First;
--              D : Distributed.Types.Dposition2d;
--              use Convert;
--           begin
--              while Has_Element (I) loop
--                 D.Position := + C_Output (J).Odom_Pose;
--                 D.Velocity := + C_Output (J).Velocity;
--                 This.Sdb.Set (Agent_Update_Key (Key (I)), D);
--
--                 J := J + 1;
--                 Next (I);
--              end loop;
--           end;
      end;
   end Do_Acquire_Poses;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
   begin
      if This.Bot.Get_Name = "" then
         return;
      end if;

      if This.Update_Pose then
         Do_Update_Pose (This);
      end if;
      if This.Acquire_Poses then
         Do_Acquire_Poses (This);
      end if;

      loop
         This.Next := This.Next + This.Period;
         Next      := This.Next;

         if Clock > Next then
            Log ("Elastic getting behind!", Warning);
         else
            exit;
         end if;
      end loop;
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Elastic;
