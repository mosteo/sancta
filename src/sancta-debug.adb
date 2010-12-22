with Agpl.Conversions; use Agpl.Conversions;

package body Sancta.Debug is

   -----------
   -- Print --
   -----------

   procedure Print (This : in Sancta.Tasks.Containers.Lists.List) is
      use Sancta.Tasks.Containers.Lists;

      procedure Print_One (I : in Cursor) is
      begin
         Log (Element (I).To_String, Agpl.Trace.Debug);
      end Print_One;

   begin
      Iterate (This, Print_One'Access);
   end Print;

   ---------------
   -- To_String --
   ---------------

   function To_String_X (This : in Num) return String is
   begin
      return To_String (Long_Long_Float (This));
   end To_String_X;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Sancta.Costs) return String is
   begin
      return To_String (Long_Long_Float (This));
   exception
      when Constraint_Error =>
         return " Too big";
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Types.Angle) return String is
   begin
      return To_String (Long_Long_Float (This), Decimals => 5);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Types.Real) return String is
   begin
      return To_String (Long_Long_Float (This), Decimals => 3);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Types.Pose) return String is
   begin
      return
        "(" &
      To_String (This.X) & "," &
      To_String (This.Y) & "," &
      To_String (This.A) & ")";
   end To_String;

end Sancta.Debug;
