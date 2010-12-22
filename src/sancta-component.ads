with Ada.Calendar;
with Ada.Containers.Indefinite_Vectors;
with Agpl.Generic_Handle;
with Agpl.Protected_Datastore;
with Agpl.Ustrings; use Agpl.Ustrings;
with Agpl.Xml;

--  pragma Warnings (Off);
--  with Agpl.Types.Ustrings; use Agpl.Types.Ustrings; -- Used in descendents!
--  pragma Warnings (On);

package Sancta.Component is

   --  This is the foundation for SANCTA components.
   --  New components may be better derived from Component.Root, which adds
   --    many utilities for data accessing.

   --   pragma Elaborate_Body;

   subtype Data is Agpl.Protected_Datastore.Object_Data;

   package Data_Handles is new Agpl.Generic_Handle
     (Data'Class,
      Agpl.Protected_Datastore."=");
   subtype Data_Handle is Data_Handles.Object;

   function Equals (L, R : Data'Class) return Boolean;
   --  Dispatch to predefined "="

   type Data_Parcel is new Data with record
      Owner : Node_Id;
      Label : Ustring;
      Datum : Data_Handle;
   end record;
   --  Used to tag some data; in e.g. the redirect_listener component.

   function Wrap (Datum : Data'Class;
                  Label : String;
                  Owner : Node_Id) return Data_Parcel;

   subtype Component_Name is String;

   subtype Comp_Config is Agpl.Xml.Node;

   type Option_Attr is new String;
   function Image (X : Option_Attr) return String; pragma Inline (Image);

   type Internal_Key is new String;
   --  These are the /internal/ name for the plugin of a input/output

   type External_Key is new Agpl.Protected_Datastore.Object_Key;
   --  These are the /external/ (datastore) key for a given input/output

   type Internal_Key_Array is
     array (Positive range <>) of access constant Internal_Key;

   package Internal_Key_Vectors is new
     Ada.Containers.Indefinite_Vectors (Positive, Internal_Key);
   type Internal_Key_Vector is new Internal_Key_Vectors.Vector with null record;

   function To_Vector (X : Internal_Key_Array) return Internal_Key_Vector;

   --  This should be an interface, but gap-2005 pukes elsewhere
   type Object (Name : access constant String)
     is abstract tagged limited null record;
   type Object_Access is access all Object'Class;

   not overriding
   procedure Stop (This : in out Object) is abstract;
   --  Finish whatever.

   not overriding
   function Is_Thread_Safe (This : Object) return Boolean is abstract;
   --  If true, will be run in its own worker.

   not overriding
   procedure Outer_Run (This : in out Object;
                        Next :    out Ada.Calendar.Time) is abstract;
   --  This will be overriden by Root component.
   --  Not intended for developer tinkering, instead use Root.Run

   not overriding
   function Is_Master (This : Object) return Boolean;
   --  Related to on demand creation, not for client use.

end Sancta.Component;
