package body Sancta.Gui.Robot_Data is

   use Agpl.Containers.String_String_Maps;

   ------------
   -- Object --
   ------------

   protected body Object is

      -------------------
      -- Get_Attribute --
      -------------------

      function Get_Attribute (Name : String) return String is
      begin
         if not Contains (Attributes, Name) then
            return "";
         else
            return Element (Find (Attributes, Name));
         end if;
      end Get_Attribute;

      -------------------
      -- Set_Attribute --
      -------------------

      procedure Set_Attribute (Name, Value : String) is
      begin
         Include (Attributes, Name, Value);
      end Set_Attribute;

      ---------------
      -- Get_Tasks --
      ---------------

      function Get_Tasks return Sancta.Tasks.Containers.Lists.List is
      begin
         return Tasks;
      end Get_Tasks;

      ---------------
      -- Set_Tasks --
      ---------------

      procedure Set_Tasks (Tasks : Sancta.Tasks.Containers.Lists.List) is
      begin
         Object.Tasks := Tasks;
      end Set_Tasks;

   end Object;

   -----------------------
   -- Get_Next_Sequence --
   -----------------------

   use type Types.Counter32;
   Seq : Types.Counter32 := 0;
   function Get_Next_Sequence return Types.Counter32 is
      Curr : constant Types.Counter32 := Seq;
   begin
      Seq := Seq + 1;
      return Curr;
   end Get_Next_Sequence;

end Sancta.Gui.Robot_Data;
