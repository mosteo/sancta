with Agpl.Strings;

package body Sancta.Tasks.Extra is

   use type Sancta.Tasks.Task_Id;

   --------------
   -- Contains --
   --------------

   function Contains (T  : Sancta.Tasks.Containers.Lists.List;
                      Id : Sancta.Tasks.Task_Id)
                      return Boolean
   is
      use Sancta.Tasks.Containers.Lists;
      I : Cursor := T.First;
   begin
      while Has_Element (I) loop
         if Element (I).Get_Id = Id then
            return True;
         end if;
         Next (I);
      end loop;
      return False;
   end Contains;

   ------------
   -- Insert --
   ------------

   procedure Insert (List : in out Containers.Lists.List;
                     Job  :        Object'Class;
                     Bfr  :        Task_Id := No_Task)
   is
      use Containers.Lists;
   begin
      if Bfr = No_Task then
         List.Append (Job);
      else
         declare
            I : Containers.Lists.Cursor := List.First;

            Target : Cursor := No_Element;

            procedure Query (T : Object'Class) is
            begin
               if T.Get_Id = Bfr then
                  Target := I;
               end if;
            end Query;

         begin
            while Has_Element (I) and not Has_Element (Target) loop
               Query_Element (I, Query'Access);
               Next (I);
            end loop;

            List.Insert (Target, Job);
         end;
      end if;
   end Insert;

   -----------
   -- Print --
   -----------

   procedure Print (List : Containers.Lists.List) is
      Pos : Positive := 1;
      procedure Print (I : Containers.Lists.Cursor) is
         use Agpl.Strings;
      begin
         Log ("Task " & Rpad (Pos'Img, 2) & ". " &
              Containers.Lists.Element (I).To_String,
              Always, Log_Section);
         Pos := Pos + 1;
      end Print;
   begin
      List.Iterate (Print'Access);
   end Print;

end Sancta.Tasks.Extra;
