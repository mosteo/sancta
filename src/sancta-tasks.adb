

with Agpl.Streams;
with Agpl.Streams.Circular_Unbounded;

--  with Ada.Text_Io; use Ada.Text_Io;
with Ada.Unchecked_Deallocation;

package body Sancta.Tasks is

   ---------------
   -- Assign_Id --
   ---------------

   procedure Assign_Id (This : in out Object) is
   begin
      This.Id := Get_New_Id;
   end Assign_Id;

   ------------
   -- Delete --
   ------------

   procedure Delete (This : in out Object_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object'Class, Object_Access);
   begin
      Free (This);
   end Delete;

   --------------
   -- Force_Id --
   --------------

   procedure Force_Id (This : in out Object; Id : in Task_Id) is
   begin
      This.Id := Id;
   end Force_Id;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (This : in Object) return Task_Id is
   begin
      return This.Id;
--        if This.Id = 0 then
--           raise Id_Error;
--        else
--           return This.Id;
--        end if;
   end Get_Id;

   ------------
   -- Get_Id --
   ------------

   function Get_Id2 (This : in Object'Class) return Task_Id is
   begin
      return This.Id;
   end Get_Id2;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property (This : in Object;
                          Key  : in Property_Key;
                          Def  : in String := "") return String
   is
      use Containers.String_String_Maps;
      I : constant Cursor := This.Properties.Find (String (Key));
   begin
      if Has_Element (I) then
         return Element (I);
      elsif Def /= "" then
         return Def;
      else
         raise Constraint_Error;
      end if;
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property (This : in out Object;
                           Key  : in     Property_Key;
                           Val  : in     String)
   is
   begin
      This.Properties.Include (String (Key), Val);
   end Set_Property;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Object) return String is
   begin
      return "Task" & Integer'Image (Integer (This.Id)) &
                                     "-" &
                                     External_Tag (Object'Class (This)'Tag);
   end To_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
   begin
      Assign_Id (This);
--      Put_Line ("Assigning ID: " & This.Id'Img);
   end Initialize;

   ---------------
   -- Serialize --
   ---------------

   function Serialize (This : in Object) return String is
   begin
      return Do_Serialize (This);
   end Serialize;

   ------------------
   -- Do_Serialize --
   ------------------

   function Do_Serialize (This : in Object'Class) return String is
      use Agpl.Streams;
      use Agpl.Streams.Circular_Unbounded;
      use type Stream_Element_Offset;
      Stream : aliased Stream_Type;
   begin
      Stream.Create;
      Object'Class'Output (Stream'Access, This);
      declare
         Str  : Stream_Element_Array (1 .. Stream.Available_Read);
         Last : Stream_Element_Offset;
      begin
         Stream.Read (Str, Last);
         pragma Assert (Last = Str'Last);

         return To_String (Str);
      end;
   end Do_Serialize;

   --------------------
   -- Do_Unserialize --
   --------------------

   function Do_Unserialize (This : in String) return Object'Class is
      use Agpl.Streams;
      use Agpl.Streams.Circular_Unbounded;
      Stream : aliased Stream_Type;
   begin
      Stream.Create;
      Stream.Write (To_Stream_Element_Array (This));
      return Object'Class'Input (Stream'Access);
   end Do_Unserialize;

   protected Counter is
      procedure Next (I : out Positive);
      procedure Set (I : in Positive);
   private
      Count : Positive := Positive'First;
   end Counter;

   protected body Counter is
      procedure Next (I : out Positive) is
      begin
         I     := Count;
         if Count /= Positive'Last then
            Count := Count + 1;
         else
            Count := Positive'First;
         end if;
      end Next;
      procedure Set (I : in Positive) is
      begin
         Count := I;
      end Set;
   end Counter;

   ----------------
   -- Get_New_Id --
   ----------------

   function Get_New_Id return Task_Id is
      I : Positive;
   begin
      Counter.Next (I);
      return Task_Id (I);
   end Get_New_Id;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Object_Access)
   is
   begin
      Item := new Object'Class'(Object'Class'Input (Stream));
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Object_Access)
   is
   begin
      Object'Class'Output (Stream, Item.all);
   end Write;

   -------------
   -- Same_Id --
   -------------

   function Same_Id (L, R : in Object'Class) return Boolean is
   begin
      return L.Id = R.Id;
   end Same_Id;

   -----------------
   -- Set_Next_Id --
   -----------------

   procedure Set_Next_Id (X : in Positive) is
   begin
      Counter.Set (X);
   end Set_Next_Id;

end Sancta.Tasks;
