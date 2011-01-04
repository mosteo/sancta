package body Sancta.Ctree.Tasks.Go is

   ------------
   -- Create --
   ------------

   function Go_Fwrd
     (Pose      : in Types.Pose;
      Use_Angle : in Boolean     := True;
      Margin_D  : in Types.Real  := 0.5;
      Margin_A  : in Types.Angle := 0.25)
      return Fwrd
   is
   begin
      return
        (Sancta.Tasks.Goto_Pose.Create (Pose, Use_Angle, Margin_D, Margin_A)
           with null record);
   end Go_Fwrd;

   ------------
   -- Create --
   ------------

   function Go_Back
     (Pose      : in Types.Pose;
      Use_Angle : in Boolean     := True;
      Margin_D  : in Types.Real  := 0.5;
      Margin_A  : in Types.Angle := 0.25)
      return Back
   is
   begin
      return
        (Sancta.Tasks.Goto_Pose.Create (Pose, Use_Angle, Margin_D, Margin_A)
           with null record);
   end Go_Back;

end Sancta.Ctree.Tasks.Go;
