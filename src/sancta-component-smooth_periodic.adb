with Agpl.Strings;
with Sancta.Component.Factory;
with Sancta.Component.Network;

package body Sancta.Component.Smooth_Periodic is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create_Ptr);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Comp_Config)
      return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name_Ptr, Config);
   begin
      This.Period.Set_Period
        (Duration'Value (This.Option (Opt_Period, Def_Period'Img)));

      declare
         Samples : constant Positive :=
                     Positive (Duration'Value (This.Option (Opt_Avg_Period, Def_Avg_Period'Img)) /
                                 This.Period.Get_Period);
      begin
         Log ("Samples for smoothing:" & Samples'Img, Informative, Log_Section);
         This.Avg := new Avg_Item.Object (Size => Samples);
      end;

      return Component.Object_Access (This);
   end Create;

   -------------
   -- Process --
   -------------

   procedure Process (This : in out Object) is
      use Agpl.Strings;
      Val : Reals.Real'Class :=
              Reals.Real'Class (This.Input (Requires_X));
   begin
      This.Avg.Push (Val.Normalize);

      Log
        ("X (Curr/Avg/Med): " &
         Rpad (To_String (Float (Val.Normalize)), 7) &
         Rpad (To_String (Float (This.Avg.Average)), 7) &
         Rpad (To_String (Float (This.Avg.Median)),  7),
         Debug, Log_Section);

      case Kinds'Value (This.Option (Opt_Kind)) is
         when Average =>
            Val := Reals.Denormalize (This.Avg.Average);
         when Median =>
            Val := Reals.Denormalize (This.Avg.Median);
      end case;

      This.Output (Provides_Y, Val);

   end Process;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
   begin
      Process (This);
      This.Period.Next (Next);
   end Run;

end Sancta.Component.Smooth_Periodic;
