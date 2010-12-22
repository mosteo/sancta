with Agpl.Strings; use Agpl.Strings;
with Agpl.Trace;   use Agpl.Trace;

package body Sancta.Distributed is

   -----------
   -- Image --
   -----------

   function Image (This : in Object_Data) return String is
   begin
      return External_Tag (Object_Data'Class (This)'Tag);
   end Image;

   function Image (K : in Object_Key) return String is
   begin
      return +Ustring (K);
   end Image;

   function Image (K : in Object_Key; M : in Object_Metadata) return String is
   begin
      return
        Image (K) & "." &
      Image (M.Owner) & "." &
      Trim (M.Local_Revision'Img) & "." &
      Trim (M.Newest_Revision'Img);
   end Image;

   function Image (Value : in Object_Entry) return String is
   begin
      return Image (Value.Key, Value.Meta);
   end Image;

   ---------------
   -- Must_Lose --
   ---------------

   function Must_Lose (This : in Object_Entry) return Boolean is
   begin
      return This.Best_Owner /= No_Node and then
             This.Cron_Best_Owner.Elapsed >= Hold_Ownership_Period;
   end Must_Lose;

   -----------
   -- Value --
   -----------

   function Value (S : in String) return Object_Key is
   begin
      return Object_Key (To_Ustring (S));
   end Value;

end Sancta.Distributed;
