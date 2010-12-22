package body Sancta.Tasks.Search_Blob_In is

   ----------------------
   -- Create_Rectangle --
   ----------------------

   function Create_Rectangle
     (Left,
      Top,
      Right,
      Bottom    : in Types.Real;
      Color     : in Types.Colors;
      Tolerance : in Types.Percent := 0.0)
      return Object
   is
   begin
      return (Sancta.Tasks.Compound.Object with
              Shape     => Rectangle,
              Color     => Color,
              Tolerance => Tolerance,
              Top       => Top,
              Left      => Left,
              Right     => Right,
              Bottom    => Bottom);
   end Create_Rectangle;

end Sancta.Tasks.Search_Blob_In;
