with Agpl.Calendar.Format;
with Agpl.Chronos;
with Agpl.Strings;
with Agpl.Ustrings; use Agpl.Ustrings;
with Sancta.Ctree.Component.Nctypes;
with Sancta.Component.Factory;
with Sancta.Component.Network;

package body Sancta.Ctree.Component.Signal_Rtwmp is

   type Object_Access is access all Object;

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
     (Config : Comp_Config;
      Env    : Environment.Object)
      return Component.Object_Access
   is
      use Agpl.Calendar.Format;

      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Id := Env.Id;
      This.Link := Sancta.Network.Layer.Object_Access
        (Component.Network.Network (This.Input (Requires_Link)).Link);

      pragma Assert (This.Link.Num_Nodes >= 1);
      This.Avgs := new Avg_Array (0 .. Rtwmp_Address (This.Link.Num_Nodes - 1));

      This.Period.Set_Period
        (Duration'Value (This.Option (Opt_Period, Def_Period'Img)));

      declare
         Samples : constant Positive :=
                     Positive (Duration'Value (This.Option (Opt_Avg_Period, Def_Avg_Period'Img)) /
                                 This.Period.Get_Period);
      begin
         Log ("Samples for RTWMP avg:" & Samples'Img, Informative, Log_Section);
         for I in This.Avgs'Range loop
            This.Avgs (I) := new Avg_Signal.Object (Size => Samples);
         end loop;
      end;

      This.Logger.Set_File (Name => "rtwmp." & Image (This.Link.Id) &
                            Datestamp (Separator => '.') & "." &
                            Timestamp & ".log");

      return Component.Object_Access (This);
   end Create;

   -------------
   -- Process --
   -------------

   procedure Process (This : in out Object) is
      use Agpl.Strings;
      use Sancta.Network.Layer;
      Link : constant Rtwmp.Object_Access := Rtwmp.Object_Access (This.Link);

      Q    : constant Quality_Matrix := Link.Get_Quality;

      procedure Output_Full_Links is
         Full_Links : Nctypes.Full_Signal (Directed_Source   => True,
                                           Directed_Query    => False,
                                           Missing_As_Broken => True);
      begin
         pragma Assert (Q'First (1) = Q'First (2));
         pragma Assert (Q'Last  (1) = Q'Last  (2));

         if This.First_Log then
            This.First_Log := False;
            declare
               Header : Ustring := +"Epoch ";
            begin
               for I in Q'Range loop
                  for J in Q'Range loop
                     ASU.Append
                       (Header,
                        Image (Link.Get_Node_Id (Rtwmp.Image (I))) & "-" &
                        Image (Link.Get_Node_Id (Rtwmp.Image (J))) & " ");
                  end loop;
               end loop;
               This.Logger.Log (+Header, Always);
            end;
         end if;

         declare
            Line : Ustring :=
                     +(To_String (Float (Agpl.Chronos.Epoch.Elapsed)) & " ");
         begin
            for I in Q'Range loop
               for J in Q'Range loop -- Both ranges must be same
                  Full_Links.Links.Set
                    (Link.Get_Node_Id (Rtwmp.Image (I)),
                     Link.Get_Node_Id (Rtwmp.Image (J)),
                     Signal_Q (Q (I, J)));
                  ASU.Append (Line,
                              To_String (Float (Q (I, J)), 1) & " ");
               end loop;
            end loop;
            This.Logger.Log (+Line, Always);
         end;

         This.Output (Provides_Raw_Full_Signal, Full_Links);
      end Output_Full_Links;
   begin
      pragma Assert
        (Q'First = This.Avgs'First and then Q'Last = This.Avgs'Last);

      for C in Q'Range (2) loop
         This.Avgs (C).Push (Q (Rtwmp.Value (Link.Get_Address (This.Id)), C));
         Log
           ("Q (Addr/Id/Curr/Avg/Med): " & Rtwmp.Image (C) & " " &
            Image (Link.Get_Node_Id (Rtwmp.Image (C))) &
            Rpad (To_String
              (Float (Q (Rtwmp.Value (Link.Get_Address (This.Id)), C))), 7) &
            Rpad (To_String (Float (This.Avgs (C).Average)), 7) &
            Rpad (To_String (Float (This.Avgs (C).Median)),  7),
            Debug, Log_Section);
      end loop;

      declare
         Raw_Result : Nctypes.Signal;
         Avg_Result : Nctypes.Signal;
         Med_Result : Nctypes.Signal;
      begin
         for I in This.Avgs'Range loop
            Raw_Result.Links.Insert (Link.Get_Node_Id (Rtwmp.Image (I)),
                                     Signal_Q (This.Avgs (I).Last));
            Avg_Result.Links.Insert (Link.Get_Node_Id (Rtwmp.Image (I)),
                                     Signal_Q (This.Avgs (I).Average));
            Med_Result.Links.Insert (Link.Get_Node_Id (Rtwmp.Image (I)),
                                     Signal_Q (This.Avgs (I).Median));
         end loop;
         This.Output (Provides_Raw_Signal, Raw_Result);
         This.Output (Provides_Avg_Signal, Avg_Result);
         This.Output (Provides_Med_Signal, Med_Result);

         Output_Full_Links;
      end;

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

end Sancta.Ctree.Component.Signal_Rtwmp;
