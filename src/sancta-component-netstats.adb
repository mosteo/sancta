with Agpl.Conversions;
with Sancta.Component.Factory;
with Sancta.Component.Network;
with Sancta.Component.Redirect_Listener;

package body Sancta.Component.Netstats is

   package SN renames Sancta.Network;

   type Object_Access is access all Object;

   type Ping is new Data with null record;

   package ACKs is new Redirect_Listener.ACKs (Object_Access);

   procedure Callback
     (This    : Object_Access;
      Key     : External_Key;
      Val     : Data'Class)
   is
      pragma Unreferenced (Val);
   begin
      pragma Assert (Key = Ping_Key);
      This.Safe.ACKd;
   end Callback;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Comp_Config)
      return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.With_Ping :=
        Boolean'Value (This.Option (Opt_Enable_Ping, Def_Enable_Ping'Img));

      if This.With_Ping then
         This.Chan := SN.Value (This.Option (Opt_Ping_Channel));
         This.Dest := +This.Option (Opt_Ping_Dest);
      end if;

      This.Link :=
        SN.Layer.Root.Object_Access
          (Network.Network (This.Input (Requires_Link)).Link);

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
   begin
      if This.With_Ping then
         This.Safe.Reset;
         ACKs.Set (This'Unchecked_Access,
                   Ping_Key,
                   Ping'(Data with null record),
                   SN.Layer.Object_Access (This.Link),
                   This.Dest,
                   This.Chan,
                   This.Period.Get_Period,
                   Callback'Access);
      end if;

      declare
         S : constant Stats := (With_Ping => This.With_Ping,
                                Roundtrip => This.Safe.Elapsed,
                                BW_In     => This.Link.Avg_Bw_In,
                                BW_Out    => This.Link.Avg_Bw_Out);
      begin
         This.Output (Provides_Stats, S);

         if Boolean'Value (This.Option (Opt_Enable_Stdout, Def_Enable_Stdout'Img)) then
            Log ("NETSTATS: " & S.To_String, Always, Log_Section);
         end if;
      end;

      This.Period.Next (Next);
   end Run;


   -----------------
   -- Safe_Object --
   -----------------

   protected body Safe_Object is

      procedure Reset is
      begin
         Timer.Reset;
      end Reset;

      procedure ACKd is
      begin
         Rtrip := Timer.Elapsed;
      end ACKd;

      function Elapsed return Duration is
      begin
         return Rtrip;
      end Elapsed;

   end Safe_Object;

   ---------------
   -- To_String --
   ---------------

   function To_String (S : Stats) return String is
      use Agpl.Conversions;
      use ASU;

      R : Ustring;
   begin
      Append (R, "BW in: " & To_Size (S.Bw_In) & "/s; ");
      Append (R, "BW out: " & To_Size (S.Bw_Out) & "/s");
      if S.With_Ping then
         Append (R, "; Ping:" & Integer (S.Roundtrip * 1000.0)'Img & "ms");
      end if;

      return +R;
   end To_String;

end Sancta.Component.Netstats;
