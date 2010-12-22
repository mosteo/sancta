with Agpl.Random;

package body T000_Types is

   ---------------
   -- Selfcheck --
   ---------------

   procedure Selfcheck (M : Check_Message) is
   begin
      if Stream_Element_Offset (M.Data'Last) /= M.Length then
         raise Constraint_Error
           with "Len mismatch:" & M.Length'Img & " /=" & M.Data (M.Data'Last)'Img;
      end if;

      for I in M.Data'Range loop
         if M.Data (I) /= Stream_Element (I rem 255) then
            raise Constraint_Error
              with "Data mismatch:" & I'Img & " /=" & M.Data (I)'Img;
         end if;
      end loop;
   end Selfcheck;

   ------------
   -- Create --
   ------------

   function Create (Min_Size : Positive := 1;
                    Max_Size : Positive := 255) return Check_Message is
      M : Check_Message
        (Stream_Element_Offset (Agpl.Random.Get_Integer (Min_Size, Max_Size)));
   begin
      for I in M.Data'Range loop
         M.Data (I) := Stream_Element (I rem 255);
      end loop;
      return M;
   end Create;


end T000_Types;
