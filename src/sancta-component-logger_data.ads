with Sancta.Component.Root;

with Agpl.Generic_Handle;
with Agpl.Protected_Datastore;
with Agpl.Xml;

with Ada.Calendar;
with Ada.Text_Io;

generic
   Component_Name : String;
   with function Image (Value : Agpl.Protected_Datastore.Object_Data'Class)
     return String;
package Sancta.Component.Logger_Data is

   pragma Elaborate_Body;

   Requires_Data : constant Internal_Key := "data";

   Option_Filename : constant Option_Attr := "filename";
   Option_Periodic : constant Option_Attr := "periodic";
   Option_Period   : constant Option_Attr := "period";
   Option_Remove_Duplicates : constant Option_Attr := "remove_duplicates";
   Option_Echo : constant Option_Attr := "echo";

   procedure Register;

private

   package Data_Handles is new Agpl.Generic_Handle
     (Data'Class, Component.Equals);

   type Object;
   type Object_Access is access all Object'Class;

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

   type Object is new Root.Object with
      record
         Filename  : access String;
         File      : Ada.Text_Io.File_Type;

         Periodic  : Boolean  := False;
         Period    : Duration := 0.1;

         Remove_Duplicates : Boolean := True;
         Prev_Data         : Data_Handles.Object;

         Echo      : Boolean := True;

         Next      : Ada.Calendar.Time := Ada.Calendar.Clock;

         --  Convenience fields:
         As_Data : access String;
      end record;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   overriding
   procedure Stop (This : in out Object);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Logger_Data;
