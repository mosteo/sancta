

--  Tasks of the network.

--  Tasks can be either primitive or compound. A primitive task is directly
--  doable by some known mean (also called operator). A compound task must be
--  decomposed in other tasks (either compound and/or primitive) until all
--  tasks are primitive. At this point we say we have a valid Plan.

private with Agpl.Containers.String_String_Maps;

with Ada.Finalization;
with Ada.Streams;

package Sancta.Tasks is

   pragma Preelaborate;

   Id_Error : exception;

   type Property_Key is new String;

   Property_Auctionable : constant Property_Key := "auctionable";

   type Object is abstract tagged private;
   type Object_Access is access Object'Class;

   type Task_Id is new Natural;
   No_Task : constant Task_Id;

   procedure Assign_Id (This : in out Object);
   --  This should be used when creating a task to assign a unique ID to it.

   procedure Set_Next_Id (X : in Positive);
   --  For debug

   function Get_Id (This : in Object) return Task_Id;
   function Get_Id2 (This : in Object'Class) return Task_Id;
   pragma Inline (Get_Id);
   --  Returns the id for this task.
   --  Raises Id_Error if the task has not been properly initialized.

   function Is_Primitive (This : in Object) return Boolean is abstract;

   function Get_Property (This : in Object;
                          Key  : in Property_Key;
                          Def  : in String := "") return String;
   --  If the default value is "" and the property doesnt exist, raise C_E.

   procedure Set_Property (This : in out Object;
                           Key  : in     Property_Key;
                           Val  : in     String);

   function To_String (This : in Object) return String;
   function Image     (This : in Object) return String renames To_String;
   --  Human readable task description.
   --  This default returns "Task #id".

   procedure Delete (This : in out Object_Access);
   --  Used to free objects of Object'Class type

   function Serialize (This : in Object) return String;
   --  String representation of all relevant task info.
   --  Default implementation calls Do_Serialize.
   --  Override if needed (pointers or so).

   function Do_Serialize (This : in Object'Class) return String;
   --  Shortcut for tasks without pointers and so.
   --  It will just use the 'Output attribute in a memory stream.

   function Do_Unserialize (This : in String) return Object'Class;
   --  Reverse operation of Do_Serialize.

   function Get_New_Id return Task_Id;
   --  Get a new unique id (thread safe).

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   :    out Object_Access);
   for Object_Access'Read use Read;

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Object_Access);
   --  Will dispatch to .all'Write
   for Object_Access'Write use Write;

   function Same_Id (L, R : in Object'Class) return Boolean;
   --  Equality by id.

   procedure Force_Id (This : in out Object; Id : in Task_Id);
   --  Forcefully assign an Id to a task
   --  Use with caution

private

   use Agpl;

   No_Task : constant Task_Id := 0;

   type Object is abstract new Ada.Finalization.Controlled with record
      Id         : Task_Id := No_Task;
      Properties : Containers.String_String_Maps.Map;
   end record;

   procedure Initialize (This : in out Object);

end Sancta.Tasks;
