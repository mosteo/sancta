package body Sancta.Auctions is

   -----------
   -- Image --
   -----------

   function Image (This : Items) return String is
   begin
      return String (Items'Class (This).Id);
   end Image;

   -----------------
   -- Readable_Id --
   -----------------

   function Readable_Id (This : Items) return String is
      Thix : Items'Class renames Items'Class (This);
   begin
      if String (Thix.Id) = Thix.Image then
         return Thix.Image;
      else
         return "Img: " & Thix.Image &
                "; Id: " & String (Thix.Id);
      end if;
   end Readable_Id;

   --------
   -- Id --
   --------

   function Id (S : String) return Auction_Id is
   begin
      return Auction_Id (Gnat.Md5.Message_Digest'(Gnat.Md5.Digest (S)));
   end Id;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Auction_Id) return Boolean is
   begin
      return String (L) < String (R);
   end "<";

   -----------
   -- Image --
   -----------

   function Image (Id : Auction_Id) return String is
   begin
      return String (Id);
   end Image;

end Sancta.Auctions;
