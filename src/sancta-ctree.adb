package body Sancta.Ctree is

   -----------
   -- Image --
   -----------

   function Image (Id : Robot_Id) return String is
   begin
      return Id'Img;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Id : String) return Robot_Id is
   begin
      return Robot_Id'Value (Id);
   end Value;

   ----------------
   -- To_Quality --
   ----------------

   function To_Quality (F : Float) return Link_Qualities is
   begin
      if F > Float (Link_Qualities'Last) then
         return Link_Qualities'Last;
      else
         return Link_Qualities (F);
      end if;
   end To_Quality;

   -----------------
   -- To_Signal_Q --
   -----------------

   function To_Signal_Q (F : Float) return Signal_Q is
   begin
      return Signal_Q (F);
   end To_Signal_Q;

end Sancta.Ctree;
