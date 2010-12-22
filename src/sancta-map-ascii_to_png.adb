--  Procedure which converts an ASCII file to a bitmap PNG.

with Agpl.Containers.String_Vectors;

with Png_Io; use Png_Io;

with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_Io;      use Ada.Text_Io;

-----------------------------
-- Sancta.Map.Ascii_To_Png --
-----------------------------
--  Everything that is not obstacle, it is free.
procedure Sancta.Map.Ascii_To_Png (In_Name  : String;
                                   Out_Name : String;
                                   Obstacle : Character_Set)
is
   Map : Agpl.Containers.String_Vectors.Vector;

   ----------
   -- Read --
   ----------

   procedure Read is
      Ascii : File_Type;
   begin
      Open (Ascii, In_File, In_Name);
      while not End_Of_File (Ascii) loop
         Map.Append (Get_Line (Ascii));
      end loop;
      Close (Ascii);
   exception
      when others =>
         if Is_Open (Ascii) then
            Close (Ascii);
         end if;
         raise;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write is
      type Sample is (Obst, Free);

      -----------
      -- Width --
      -----------

      function Width (This : Agpl.Containers.String_Vectors.Vector)
                      return Natural
      is
         Max : Natural := 0;
      begin
         for I in This.First_Index .. This.Last_Index loop
            Max := Natural'Max (Max, This.Element (I)'Length);
         end loop;
         return Max;
      end Width;

      -----------------
      -- Grey_Sample --
      -----------------

      function Grey_Sample (I    : Agpl.Containers.String_Vectors.Vector;
                            R, C : Coordinate) return Sample is
      begin
         pragma Assert (I.Element (R + 1)'First = 1);

         if I.Element (R + 1)'Last < C + 1 then
            return Free;
         elsif Is_In (I.Element (R + 1) (C + 1), Obstacle) then
            return Obst;
         else
            return Free;
         end if;
      end Grey_Sample;

      procedure Write_Png is new
        Write_Png_Type_0 (Agpl.Containers.String_Vectors.Vector,
                          Sample,
                          Grey_Sample);
   begin
      Put_Line ("Size:" & Natural (Map.Length)'Img & Width (Map)'Img);
      Write_Png (Out_Name,
                 Map,
                 Y         => Natural (Map.Length),
                 X         => Width (Map),
                 Bit_Depth => One);
   end Write;
begin
   Read;
   Write;
end Sancta.Map.Ascii_To_Png;
