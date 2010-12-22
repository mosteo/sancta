---------------------------------
-- Sancta.Map.Create_Wavefront --
---------------------------------

function Sancta.Map.Create_Wavefront
  (To       : Location'Class;
   Map      : Sancta.Map.Object'Class;
   From     : Location_Handle.Object := Location_Handle.Null_Object)
   return  Loc_Cost_Maps.Map
is

   Pending  : Location_Lists.List;
   Visited  : Loc_Cost_Maps.Map;
   Use_From : constant Boolean := From.Is_Valid;

begin

   --  Startup, we are at 0 cost.
   Visited.Include (To, 0.0);
   Pending.Append (To);

   loop
      --  Exit if origin given and found
      exit when Use_From and then Visited.Contains (From.Ref.all);

      --  Error if origin given and not found
      if Use_From and then Pending.Is_Empty then
         raise Constraint_Error with "Unreachable goal";
      end if;

      --  Exit if full wave front created
      exit when Pending.Is_Empty;

      declare
         Pos  : constant Location'Class          := Pending.First_Element;
         Near : constant Location_Vectors.Vector := Neighbors (Map, Pos);
         Cost : constant Costs                   := Visited.Element (Pos);
      begin
         Pending.Delete_First;

         for I in Near.First_Index .. Near.Last_Index loop
            declare
               Step : constant Location'Class := Near.Element (I);
            begin
               if Map.Is_Known (Step) then
                  if (not Visited.Contains (Step)) and then
                    Get_Cost (Map, Map.Get_At (Step)) < Infinite
                  then
                     Visited.Insert
                       (Step, Cost + Get_Cost (Map, Map.Get_At (Step)));
                     Pending.Append (Step);
--                       Put_Line
--                         ("Adding " & Step.Image & " C:" &
--                          Integer'Image
--                            (Integer (Cost +
--                               Get_Cost (Map, Map.Get_At (Step)))));
                  end if;
               end if;
            end;
         end loop;
      end;
   end loop;

--     declare
--        use Bitmap, Agpl.Strings;
--        M : Bitmap.Object renames Bitmap.Object (Map);
--     begin
--        New_Line; New_Line; New_Line;
--        for R in M.Get_Data'Range loop
--           for C in M.Get_Data'Range (2) loop
--              if M.Get_At (C, R) = Obstacle then
--                 Put ("XXX");
--              elsif Visited.Contains (Bit_Location'(C, R)) then
--                 Put (Rpad (Integer (Visited.Element (Bit_Location'(C, R)))'Img, 3));
--              else
--                 Put (" . ");
--              end if;
--           end loop;
--           New_Line;
--        end loop;
--     end;

   return Visited;
end Sancta.Map.Create_Wavefront;
