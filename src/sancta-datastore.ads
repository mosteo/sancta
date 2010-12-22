with Agpl.Interfaces.C.Types;
with Sancta.Network.Layer;
with Sancta.Located_Agent;
with Sancta.Types;

with Agpl.Protected_Datastore;

package Sancta.Datastore is

--     pragma Preelaborate;

   subtype Object_Data is Agpl.Protected_Datastore.Object_Data;

   type Bool (Value : Boolean) is new Agpl.Protected_Datastore.Object_Data
   with null record;

   type Int (Value : Integer) is new Agpl.Protected_Datastore.Object_Data
   with null record;

   type Pose is new Agpl.Protected_Datastore.Object_Data with record
      Pose : Types.Pose;
   end record;

   type Dstring (Length : Natural) is new Agpl.Protected_Datastore.Object_Data
   with record
      Str : String (1 .. Length);
   end record;

   function New_Dstring (S : String) return Dstring;

   type Network_Layer (Ref : access Network.Layer.Object'Class) is new
     Agpl.Protected_Datastore.Object_Data with null record;

   type Robot (Ref : access Located_Agent.Object'Class) is new
     Agpl.Protected_Datastore.Object_Data with null record;

   --  The global datastore [NO LONGER USED]
   --  Object : Agpl.Protected_Datastore.Object;

   function Object (Id : Node_Id) return Agpl.Protected_Datastore.Object_Access;
   --  There's one datastore per node id. This allows several nodes in one exec.

   --  Some C types for interfacing

   type Dc_Int is new Agpl.Protected_Datastore.Object_Data with record
      Val : Agpl.Interfaces.C.Types.Int;
   end record;

   type Dc_Double is new Agpl.Protected_Datastore.Object_Data with record
      Val : Agpl.Interfaces.C.Types.Double;
   end record;

private

   package C renames Agpl.Interfaces.C.Types;

   --  Glue functions to allow stores from C/C++

   procedure Set_Double (Id  : C.Chars_Ptr;
                         Key : C.Chars_Ptr;
                         Val : C.Double);
   pragma Export (C, Set_Double, "sancta__datastore__set_double");

   procedure Set_Int (Id  : C.Chars_Ptr;
                      Key : C.Chars_Ptr;
                      Val : C.Int);
   pragma Export (C, Set_Int, "sancta__datastore__set_int");

   procedure Set_String (Id  : C.Chars_Ptr;
                         Key : C.Chars_Ptr;
                         Val : C.Chars_Ptr);
   pragma Export (C, Set_String, "sancta__datastore__set_string");

   function Get_Double (Id  : C.Chars_Ptr;
                        Key : C.Chars_Ptr) return C.Double;
   pragma Export (C, Get_Double, "sancta__datastore__get_double");

   function Get_Int (Id  : C.Chars_Ptr;
                     Key : C.Chars_Ptr) return C.Int;
   pragma Export (C, Get_Int, "sancta__datastore__get_int");

   Max_Len : constant := 256;
   subtype Datastore_C_String is C.Char_Array (1 .. Max_Len);

   procedure Get_String (Id  :     C.Chars_Ptr;
                         Key :     C.Chars_Ptr;
                         Max :     C.Int;
                         Val : out Datastore_C_String);
   pragma Precondition (C."<=" (Max, Max_Len));
   --  Val must be preallocated with enough room!
   pragma Export (C, Get_String, "sancta__datastore__get_string");

   function Contains (Id : C.Chars_Ptr; Key : C.Chars_Ptr) return C.Int;
   pragma Export (C, Contains, "sancta__datastore__contains");
   --  1 true, 0 false

end Sancta.Datastore;
