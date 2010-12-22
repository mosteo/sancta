--  Some utilities that can be used as a root class for any derived component
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Finalization;
with Agpl.Monitor;
with Sancta.Component.Environment;

package Sancta.Component.Root is

   Log_Section : constant String := "sancta.component.root";

   type Object (Name   : access constant String;
                Config :                 Comp_Config) is
   abstract new Component.Object with private;

   type Object_Access is access all Object'Class;

   --  OVERRIDABLES
   --  The ones that you may want to override

   not overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is null;
   --  Override this if you subscribe to some key...

   not overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);
   --  This default does nothing but give a ultra late next

   overriding
   procedure Stop (This : in out Object) is null;

   overriding
   function Is_Thread_Safe (This : Object) return Boolean;
   --  If true, will be run in its own worker.
   --  Defaults to Sancta.Options.Multithreaded_Events

   not overriding
   function Requires_Mutex (This : Object) return Boolean;
   --  If true, calls to Run and Key_Stored will be mutually exclusively run
   --  Defaults to Sancta.Options.Protected_Events

   --  This has to be rethough... Output calls propagate to other plugins via
   --  the Key_Stored callback... so the two of them become coupled.
   --  Somehow they have to be decoupled in these cases.
   --  Otherwise, plugins using protected types get Potentially_Blocking excepts.

   --  END OF OVERRIDABLES

   not overriding
   function Get_Id (This : Object) return Node_Id;

   not overriding
   procedure Subscribe (This : in out Object;
                        Key  : in     Internal_Key);
   --  Listen for stores of a certain key.

   not overriding
   procedure Unsubscribe (This : in out Object;
                          Key  : in     Internal_Key);
   --  Remove from listeners

   not overriding
   function Input (This : Object;
                   Key  : Internal_Key) return Data'Class;

   not overriding
   function Input (This : Object;
                   Key  : Internal_Key;
                   I    : Natural) return Data'Class;
   --  Append a indexing suffix to the key

   not overriding
   function Input (This : Object;
                   Key  : External_Key) return Data'Class;
   --  Note the *external key*. This is useful only in special cases where
   --  we don't want translation (see e.g. Proxy component)

   not overriding
   procedure Output (This : Object;
                     Key  : Internal_Key;
                     Val  : Data'Class);

   not overriding
   procedure Output (This : Object;
                     Key  : Internal_Key;
                     I    : Natural;
                     Val  : Data'Class);

   not overriding
   procedure Output (This : Object;
                     Key  : External_Key;
                     Val  : Data'Class);
   --  Note the *external key*. This is useful only in special cases where
   --  we don't want translation (see e.g. redirect_listener component)

   not overriding
   function Provided (This : Object;
                      Key  : Internal_Key) return Boolean;
   --  Says if the internal/external key pairing is given
   --  Says nothing about actual stored (or not) values

   not overriding
   function Exists (This : Object;
                    Key  : Internal_Key) return Boolean;
   --  Says if the external name for this key is already stored in datastore.
   --  I.e. if some value has been stored already

   not overriding
   function Provided_And_Exists (This : Object;
                                 Key  : Internal_Key) return Boolean;

   not overriding
   procedure Diagnose_Missing (This : Object;
                               Key  : Internal_Key);

   not overriding
   procedure Verify (This : Object; Key : Internal_Key)
                     renames diagnose_missing;
   --  Raise exception

   not overriding
   function Exists (This : Object;
                    Keys : Internal_Key_Array) return Boolean;

   not overriding
   procedure Diagnose_Missing (This : Object;
                               Keys : Internal_Key_Array);
   --  Raise an exception with the missing key as information

   not overriding
   procedure Verify (This : Object;
                     Keys : Internal_Key_Array) renames Diagnose_Missing;

   --  Generics for direct use of some derived type.

   generic
      type Xt (<>) is new Data with private;
   function Generic_Input (This : Object'Class;
                           Key  : Internal_Key) return Xt;

   generic
      type Xt (<>) is new Data with private;
   procedure Generic_Output (This : Object'Class;
                             Key  : Internal_Key;
                             Val  : Xt);

   function Get_Config (This : Object'Class) return Agpl.Xml.Node;

   --  Retrieval of configuration options
   --  Both recognize "$id" as an special attribute which will instead return
   --    the proper node id under which the component is being run.

   function Option (This : Object;
                    Attr : Option_Attr;
                    Def  : String) return String;

   function Option (This : Object;
                    Attr : Option_Attr) return String;
   --  If missing, exception

   function Exists (This : Object;
                    Attr : Option_Attr) return Boolean;

   procedure Verify (This : Object; Attr : Option_Attr);
   --  Raise exception if Attr not supplied.

   generic
      type Xt (<>) is private;
      with function Image (X : Xt) return String is <>;
      with function Value (S : String) return Xt is <>;
   function Get_Option (This : Object'Class;
                        Attr : Option_Attr;
                        Def  : Xt) return Xt;

   --  Manual addition of correspondences.
   procedure Add_Correspondence (This : in out Object;
                                 Ikey :        Internal_Key;
                                 Ekey :        External_Key);

   --  Key manipulation. Should be rarely necessary

   not overriding
   function To_External (This : in Object;
                         Data : in Internal_Key) return External_Key;

   not overriding
   function Ekey (This : in Object;
                  Data : in Internal_Key) return External_Key
                  renames   To_External;

   not overriding
   function To_Internal (This : in Object;
                         Data : in External_Key) return Internal_Key;

   not overriding
   function Ikey (This : in Object;
                  Data : in External_Key) return Internal_Key
                  renames   To_Internal;

   procedure Clear_Node_Correspondences;
   procedure Check_Node_Correspondences;
   --  For checking that some outputs/inputs are mismatched

   function Translate_Attribute (Elem : Agpl.Xml.Node;
                                 Attr : String;
                                 Def  : String;
                                 Id   : Node_Id;
                                 Env  : Environment.Object) return String;
   --  Apply translation of $vars

   overriding
   procedure Outer_Run (This : in out Object;
                        Next :    out Ada.Calendar.Time);

   --  For debug:
   not overriding
   procedure Print_Correspondences (This : Object);

   --  ********** INTERNAL MACHINERY **********  --
   --  Don't use, will be eventually removed
   procedure Set_Environment (Env : Environment.Object);

private

   procedure Add_Correspondences (This : in out Object;
                                  Opts : in     Agpl.Xml.Node);
   --  Set up two hash tables with internal-external and viceversa pairings

   package I_E_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Internal_Key, External_Key);

   package E_I_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (External_Key, Internal_Key);

   type Object_Listener (Parent : access Object) is limited new
     Agpl.Protected_Datastore.Key_Listener with null record;

   type Object_Preparer (Parent : access Object'Class) is limited new
     Ada.Finalization.Limited_Controlled with null record;

   type Object (Name   : access constant String;
                Config :                 Comp_Config)
   is abstract new Component.Object (Name => Name) with
      record
         Env      : Environment.Object;

         Prepared : Boolean := False;

         I_E_Keys : I_E_Maps.Map;
         E_I_Keys : E_I_Maps.Map;

         Listener : aliased Object_Listener (Object'Access);
         Preparer :         Object_Preparer (Object'Access);

         Mutex    : aliased Agpl.Monitor.Counting_Semaphore;
      end record;

   procedure Initialize (This : in out Object_Preparer);

   procedure On_Key_Stored (This  : in out Object_Listener;
                            Key   : in     Agpl.Protected_Datastore.Object_Key;
                            Value : in     Agpl.Protected_Datastore.Object_Data'Class);

end Sancta.Component.Root;
