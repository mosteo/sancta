with Agpl.Random;

package body Sancta.Map.Utils is

   ------------------
   -- Remove_Loops --
   ------------------

   function Remove_Loops (P : Path) return Path is
      use Paths;
      I : Cursor := P.Last;
   begin
      while Has_Element (I) loop
         declare
            J : Cursor := P.First;
         begin
            while Has_Element (J) and then J /= I loop
               if Element (J) = Element (I) then
                  declare
                     Prefix : constant Path :=
                       Path_Fields.Slice (P, P.First_Element, Element (J));
                     Suffix : constant Path :=
                       Path_Fields.Tail (P, Element (I));
                     Delooped : Path := Prefix;
                  begin
                     Log ("REMOVING ONE LOOOOOP", Debug, Log_Section);
                     Append (Delooped, Suffix);
                     Log (Image (P), Debug, Log_Section);
                     Log ("Path/Prefix/Suffix/Delooped:" &
                          P.Length'Img & Prefix.Length'Img &
                          Suffix.Length'Img & Delooped.Length'Img,
                          Debug, Log_Section);
                     return Remove_Loops (Delooped);
                  end;
               end if;
               Next (J);
            end loop;
            Previous (I);
         end;
      end loop;

      return P;
   end Remove_Loops;

   ---------------------
   -- Random_Location --
   ---------------------

   function Random_Location
     (M     : Object'Class;
      Valid : access function (O : Observation'Class) return Boolean)
      return Location'Class
   is
      V : Location_Vectors.Vector;

      procedure Get_Valid (I : Loc_Obs_Maps.Cursor) is
      begin
         if Valid (Loc_Obs_Maps.Element (I)) then
            V.Append (Loc_Obs_Maps.Key (I));
         end if;
      end Get_Valid;
   begin
      M.Loc_Obs.Iterate (Get_Valid'Access);
      if V.Is_Empty then
         raise Constraint_Error with "No candidate locations";
      else
         Log ("Random_Location:" & V.Length'Img & " found", Debug, Log_Section);
      end if;

      return V.Element (Agpl.Random.Get_Integer (V.First_Index, V.Last_Index));
   end Random_Location;

   ---------------
   -- Draw_Path --
   ---------------

   procedure Draw_Path (P :        Path;
                        D : in out Agpl.Drawing.Drawer'Class)
   is
   begin
      if P.Is_Empty then
         return;
      end if;
      declare
         Poses : constant Sancta.Containers.Pose_Vectors.Vector :=
                   P.First_Element.Get_Map.To_Poses (P);
      begin
         for I in Poses.First_Index + 1 .. Poses.Last_Index loop
            D.Draw_Line
              (Float (Poses.Element (I - 1).X),
               Float (Poses.Element (I - 1).Y),
               Float (Poses.Element (I + 0).X),
               Float (Poses.Element (I + 0).Y));
         end loop;
      end;
   end Draw_Path;

   ---------------
   -- Draw_Pose --
   ---------------

   procedure Draw_Pose (P : Types.Pose;
                        D : in out Agpl.Drawing.Drawer'Class;
                        Size : Float := 0.5)
   is
   begin
      D.Fill_Rectangle (+P.X - Size / 2.0, +P.Y - Size / 2.0,
                        +P.X + Size / 2.0, +P.Y + Size / 2.0);
   end Draw_Pose;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Path_Drawable;
                   Into : in out Agpl.Drawing.Drawer'Class)
   is
   begin
      Draw_Path (This.P, Into);
   end Draw;

   -----------------------
   -- New_Path_Drawable --
   -----------------------

   function New_Path_Drawable (P : Path) return Path_Drawable is
   begin
      return (P => P);
   end New_Path_Drawable;

   -------------
   -- To_Task --
   -------------

   function To_Task (L : Location'Class) return Sancta.Tasks.Goto_Pose.Object is
   begin
      return Sancta.Tasks.Goto_Pose.Create (L.Get_Map.Nearest_Pose (L));
   end To_Task;

   -------------
   -- To_List --
   -------------

   function To_List (V : Location_Vectors.Vector) return Tc.Lists.List is
      L : Tc.Lists.List;
      procedure Append (I : Location_Vectors.Cursor) is
      begin
         L.Append (To_Task (Location_Vectors.Element (I)));
      end Append;
   begin
      V.Iterate (Append'Access);
      return L;
   end To_List;

end Sancta.Map.Utils;
