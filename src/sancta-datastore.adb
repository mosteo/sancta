with Ada.Containers.Ordered_Maps,
     Interfaces.C,
     Interfaces.C.Strings;

package body Sancta.Datastore is

   package Store_Maps is new Ada.Containers.Ordered_Maps
     (Node_Id,
      Agpl.Protected_Datastore.Object_Access,
      "<",
      Agpl.Protected_Datastore."=");

   Stores : Store_Maps.Map;

   -----------------
   -- New_Dstring --
   -----------------

   function New_Dstring (S : String) return Dstring is
   begin
      return (S'Length, S);
   end New_Dstring;

   ------------
   -- Object --
   ------------

   function Object (Id : Node_Id) return Agpl.Protected_Datastore.Object_Access
   is
      use Store_Maps;
      I : constant Cursor := Stores.Find (Id);
   begin
      if Has_Element (I) then
         return Element (I);
      else
         declare
            Store : constant Agpl.Protected_Datastore.Object_Access :=
                      new Agpl.Protected_Datastore.Object;
         begin
            Stores.Include (Id, Store);
            return Store;
         end;
      end if;
   end Object;

   function "+" (Id : C.Chars_Ptr) return Node_Id; pragma Inline ("+");
   function "+" (Id : C.Chars_Ptr) return Node_Id is
   begin
      return Value (Standard.Interfaces.C.Strings.Value (Id));
   end "+";

   function "+" (Id : C.Chars_Ptr) return String
     renames Standard.Interfaces.C.Strings.Value;

   ----------------
   -- Set_Double --
   ----------------

   procedure Set_Double (Id  : C.Chars_Ptr;
                         Key : C.Chars_Ptr;
                         Val : C.Double)
   is
   begin
      Object (+Id).Set (+Key, Dc_Double'(Val => Val));
   end Set_Double;

   -------------
   -- Set_Int --
   -------------

   procedure Set_Int (Id  : C.Chars_Ptr;
                      Key : C.Chars_Ptr;
                      Val : C.Int)
   is
   begin
      Object (+Id).Set (+Key, Dc_Int'(Val => Val));
   end Set_Int;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (Id  : C.Chars_Ptr;
                         Key : C.Chars_Ptr;
                         Val : C.Chars_Ptr)
   is
      Str : constant String := +Val;
   begin
      Object (+Id).Set (+Key, Dstring'(Str'Length, Str));
   end Set_String;

   ----------------
   -- Get_Double --
   ----------------

   function Get_Double (Id  : C.Chars_Ptr;
                        Key : C.Chars_Ptr) return C.Double is
   begin
      return Dc_Double (Object (+Id).Get (+Key)).Val;
   end Get_Double;

   -------------
   -- Get_Int --
   -------------

   function Get_Int (Id  : C.Chars_Ptr;
                     Key : C.Chars_Ptr) return C.Int is
   begin
      return Dc_Int (Object (+Id).Get (+Key)).Val;
   end Get_Int;

   ----------------
   -- Get_String --
   ----------------

   procedure Get_String (Id  :     C.Chars_Ptr;
                         Key :     C.Chars_Ptr;
                         Max :     C.Int;
                         Val : out Datastore_C_String)
   is
      pragma Unreferenced (Max); -- Used in spec precondition
      Str   : constant String := Dstring (Object (+Id).Get (+Key)).Str;
      Count : Standard.Interfaces.C.size_t;
   begin
      Standard.Interfaces.C.To_C (Str, Val, Count);
   end Get_String;

   --------------
   -- Contains --
   --------------

   function Contains (Id : C.Chars_Ptr; Key : C.Chars_Ptr) return C.Int is
   begin
      if Object (+Id).Contains (+Key) then
         return 1;
      else
         return 0;
      end if;
   end Contains;

end Sancta.Datastore;
