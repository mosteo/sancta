--  Some utilities that can be used as a root class for any derived component

with Agpl.Containers.String_Sets;
with Agpl.Protected_Value;
with Agpl.Strings;
with Agpl.Strings.Fields;

with Sancta.Datastore;
with Sancta.Options;

package body Sancta.Component.Root is

   use Agpl;

   package Protected_Environment is new
     Agpl.Protected_Value (Environment.Object);

   Current_Environment : Protected_Environment.Object;

   -----------------
   -- Defined_Ios --
   -----------------

   protected Defined_Ios is
      procedure Clear;
      procedure Include_Input (S : String);
      procedure Include_Output (S : String);
      procedure Check_Correspondences;
   private
      Defined_Inputs  : Agpl.Containers.String_Sets.Set;
      Defined_Outputs : Agpl.Containers.String_Sets.Set;
   end Defined_Ios;

   -----------------
   -- Defined_Ios --
   -----------------

   protected body Defined_Ios is

      procedure Clear is
      begin
         Defined_Inputs.Clear;
         Defined_Outputs.Clear;
      end Clear;

      procedure Include_Input (S : String) is
      begin
         Defined_Inputs.Include (S);
      end Include_Input;

      procedure Include_Output (S : String) is
      begin
         Defined_Outputs.Include (S);
      end Include_Output;

      procedure Check_Correspondences is
         use Agpl.Containers.String_Sets;
         procedure Check_Input (I : Cursor) is
         begin
            if not Defined_Outputs.Contains (Element (I)) then
               Log ("Input [" & Element (I) & "] not provided by any output!",
                    Warning, Log_Section);
            end if;
         end Check_Input;
         procedure Check_Output (I : Cursor) is
         begin
            if not Defined_Inputs.Contains (Element (I)) then
               Log ("Output [" & Element (I) & "] not used by any input!",
                    Warning, Log_Section);
            end if;
         end Check_Output;
      begin
         Defined_Inputs.Iterate  (Check_Input'Access);
         Defined_Outputs.Iterate (Check_Output'Access);
      end Check_Correspondences;
   end Defined_Ios;

   --------------------------------
   -- Clear_Node_Correspondences --
   --------------------------------

   procedure Clear_Node_Correspondences is
   begin
      Defined_Ios.Clear;
   end Clear_Node_Correspondences;

   --------------------------------
   -- Check_Node_Correspondences --
   --------------------------------

   procedure Check_Node_Correspondences is
   begin
      Defined_Ios.Check_Correspondences;
   end Check_Node_Correspondences;

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment (Env : Environment.Object) is
   begin
      Current_Environment.set (Env);
   end Set_Environment;

   ------------------------
   -- Add_Correspondence --
   ------------------------

   procedure Add_Correspondence (This : in out Object;
                                 Ikey :        Internal_Key;
                                 Ekey :        External_Key)
   is
   begin
      This.I_E_Keys.Insert (Ikey, Ekey);
      This.E_I_Keys.Insert (Ekey, Ikey);
   end Add_Correspondence;

   -------------------------
   -- Add_Correspondences --
   -------------------------

   procedure Add_Correspondences (This : in out Object;
                                  Opts : in     Xml.Node)
   is
      procedure Add_Element (Element : in String) is
         Equivs : constant Xml.Node_Array := Xml.Get_All (Element, Opts);
      begin
         for I in Equivs'Range loop
            This.Add_Correspondence
              (Internal_Key (Xml.Get_Attribute (Equivs (I), "data", "")),
               External_Key (Xml.Get_Attribute (Equivs (I), "as",   "")));

            if Element = "provides" then
               Defined_Ios.Include_Output
                 (Xml.Get_Attribute (Equivs (I), "as",   ""));
            elsif Element = "requires" then
               Defined_Ios.Include_Input
                 (Xml.Get_Attribute (Equivs (I), "as",   ""));
            else
               raise Program_Error with "requires/provides/unknown?";
            end if;
         end loop;
      end Add_Element;

   begin
      Add_Element ("provides");
      Add_Element ("requires");
   end Add_Correspondences;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (This : Object) return Node_Id is
   begin
      return This.Env.Id;
   end Get_Id;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
      pragma Assert (This.Prepared);
   begin
      Next := Clock + 60.0 * 60.0 * 24.0;
   end Run;

   ---------------
   -- Outer_Run --
   ---------------

   procedure Outer_Run (This : in out Object;
                        Next :    out Ada.Calendar.Time)
   is
      Sem : access Agpl.Monitor.Counting_Semaphore;
   begin
      if Object'Class (This).Requires_Mutex then
         Sem := This.Mutex'Access;
      end if;

      declare
         Mutex : Agpl.Monitor.Object (Sem); pragma Unreferenced (Mutex);
      begin
         Object'Class (This).Run (Next);
      end;
   end Outer_Run;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (This : in out Object;
                        Key  : in     Internal_Key)
   is
   begin
      if This.I_E_Keys.Contains (Key) then
         Agpl.Protected_Datastore.Listen
           (Datastore.Object (This.Env.Id).all,
            String (This.Ekey (Key)),
            This.Listener'Unchecked_Access);
      else
         Log ("Warning: dropping subscription for ungiven internal key:" &
              String (Key),
              Warning, Log_Section);
      end if;
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe (This : in out Object;
                          Key  : in     Internal_Key) is
   begin
      if This.I_E_Keys.Contains (Key) then
         Agpl.Protected_Datastore.Unlisten
           (Datastore.Object (This.Env.Id).all,
            String (This.Ekey (Key)),
            This.Listener'Unchecked_Access);
      else
         Log ("Warning: dropping unsubscription for ungiven internal key:" &
              String (Key),
              Warning, Log_Section);
      end if;
   end Unsubscribe;

   -------------------
   -- On_Key_Stored --
   -------------------

   procedure On_Key_Stored (This  : in out Object_Listener;
                            Key   : in     Agpl.Protected_Datastore.Object_Key;
                            Value : in     Agpl.Protected_Datastore.Object_Data'Class)
   is
      pragma Assert (This.Parent.Prepared);
      Sem : access Agpl.Monitor.Counting_Semaphore;
   begin
      if Object'Class (This.Parent.all).Requires_Mutex then
         Sem := This.Parent.Mutex'Access;
      end if;

      declare
         Mutex : Agpl.Monitor.Object (Sem); pragma Unreferenced (Mutex);
      begin
         Object'Class (This.Parent.all).Key_Stored
           (This.Parent.Ikey (External_Key (Key)),
            Value);
      end;
   exception
      when others =>
         Log ("Class: " & External_Tag (Object'Class (This.Parent.all)'Tag),
              Error, Log_Section);
         raise;
   end On_Key_Stored;

   --------------------
   -- Is_Thread_Safe --
   --------------------

   function Is_Thread_Safe (This : Object) return Boolean is
      pragma Unreferenced (This);
   begin
      return Options.Concurrent_Events.Value;
   end Is_Thread_Safe;

   --------------------
   -- Requires_Mutex --
   --------------------

   function Requires_Mutex (This : Object) return Boolean is
      pragma Unreferenced (This);
   begin
      return Options.Protected_Events.Value;
   end Requires_Mutex;

   -----------
   -- Input --
   -----------

   function Input (This : Object;
                   Key  : Internal_Key) return Data'Class
   is
   begin
      return Datastore.Object (This.Env.Id).Get (String (This.Ekey (Key)));
   end Input;

   -----------
   -- Input --
   -----------

   function Input (This : Object;
                   Key  : Internal_Key;
                   I    : Natural) return Data'Class
   is
      use Agpl.Strings;
   begin
      --  Will mean a difference only if used during component construction.
      --  OTOH, it imposes a logarithmic overhead for each call to this. Oh well.
      Defined_Ios.Include_Input (String (This.Ekey (Key)) & Trim (I'Img));
      Log ("INCLUDING: " & String (This.Ekey (Key)) & Trim (I'Img), Always);

      return This.Input (Key & Internal_Key (Trim (I'Img)));
   end Input;

   -----------
   -- Input --
   -----------

   function Input (This : Object;
                   Key  : External_Key) return Data'Class is
   begin
      return Datastore.Object (This.Env.Id).Get (String (Key));
   end Input;

   -------------------
   -- Generic_Input --
   -------------------

   function Generic_Input (This : Object'Class;
                           Key  : Internal_Key) return Xt
   is
   begin
      return Xt (Input (This, Key));
   end Generic_Input;

   ------------
   -- Output --
   ------------

   procedure Output (This : Object;
                     Key  : Internal_Key;
                     Val  : Data'Class)
   is
   begin
      if This.I_E_Keys.Contains (Key) then
         Log ("OUT: " & This.Name.all & ": " &
              String (Key) & " --> " & String (This.Ekey (Key)),
              Debug, Log_Section);
         Datastore.Object (This.Env.Id).Set (String (This.Ekey (Key)), Val);
      else
         null; -- Not needed any more, since this is checked at startup.
--           Log ("Warning: dropping ungiven output " & String (Key),
--                Warning, Log_Section);
      end if;
   end Output;

   procedure Output (This : Object;
                     Key  : Internal_Key;
                     I    : Natural;
                     Val  : Data'Class)
   is
      use Agpl.Strings;
   begin
      This.Output (Key & Internal_Key (Trim (I'Img)), Val);
   end Output;

   procedure Output (This : Object;
                     Key  : External_Key;
                     Val  : Data'Class) is
   begin
      Log ("OUT: " & This.Name.all & ": " &
           "(Ungiven) --> " & String (Key),
           Debug, Log_Section);
      Datastore.Object (This.Env.Id).Set (String (Key), Val);
   end Output;

   --------------------
   -- Generic_Output --
   --------------------

   procedure Generic_Output (This : Object'Class;
                             Key  : Internal_Key;
                             Val  : Xt) is
   begin
      Output (This, Key, Val);
   end Generic_Output;

   ------------
   -- Exists --
   ------------

   function Exists (This : Object;
                    Key  : Internal_Key) return Boolean is
   begin
      return Datastore.Object (This.Env.Id).Contains (String (This.Ekey (Key)));
   end Exists;

   ------------
   -- Exists --
   ------------

   function Exists (This : Object;
                    Keys : Internal_Key_Array) return Boolean
   is
   begin
      for I in Keys'Range loop
         if not This.Exists (Keys (I).all) then
            return False;
         end if;
      end loop;
      return True;
   end Exists;

   ----------------------
   -- Diagnose_Missing --
   ----------------------

   procedure Diagnose_Missing (This : Object;
                               Key  : Internal_Key) is
   begin
      This.Diagnose_Missing (Internal_Key_Array'(1 => Key'Unrestricted_Access));
   end Diagnose_Missing;

   ----------------------
   -- Diagnose_Missing --
   ----------------------

   procedure Diagnose_Missing (This : Object;
                               Keys : Internal_Key_Array)
   is
   begin
      for I in Keys'Range loop
         if not This.Exists (Keys (I).all) then
            raise Constraint_Error
              with "Missing internal key: " & String (Keys (I).all);
         end if;
      end loop;
   end Diagnose_Missing;

   -----------------
   -- To_External --
   -----------------

   function To_External (This : in Object;
                         Data : in Internal_Key) return External_Key is
   begin
      return This.I_E_Keys.Element (Data);
   exception
      when others =>
         Log ("Missing external_to_internal mapping for ekey: " & String (Data),
              Error);
         Log ("At component: " & This.Name.all, Error);
         This.Print_Correspondences;
         raise;
   end To_External;

   -----------------
   -- To_Internal --
   -----------------

   function To_Internal (This : in Object;
                         Data : in External_Key) return Internal_Key is
   begin
      return This.E_I_Keys.Element (Data);
   exception
      when others =>
         Log ("Missing external_to_internal mapping for ekey: " & String (Data),
              Error);
         Log ("At component: " & This.Name.all, Error);
         This.Print_Correspondences;
         raise;
   end To_Internal;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object_Preparer) is
   begin
      if not This.Parent.Prepared then
         This.Parent.Env := Current_Environment.Get;

         --  Reset global environment, we don't need it any more.
         --  This will trigger any latent bug...
         --           Current_Environment.Set ((others => <>));
         --  NO, DOESN'T WORK. Component Helpers may mess things up.
         --  Will have to wait for the proper fix

         This.Parent.Add_Correspondences (This.Parent.Config);
         This.Parent.Prepared := True;
      end if;
   end Initialize;

   -------------------------
   -- Translate_Attribute --
   -------------------------

   function Translate_Attribute (Elem : Agpl.Xml.Node;
                                 Attr : String;
                                 Def  : String;
                                 Id   : Node_Id;
                                 Env  : Environment.Object) return String
   is
      use Agpl.Strings;
      use Agpl.Strings.Fields;
      use Agpl.Xml;

      -------------------
      -- Translate_Key --
      -------------------

      function Translate_Key (Key : String) return String is
         --  Obtain the value of this particular key
      begin
         if L (Key) = "$id" then
            return Image (Id);
         else
            if Env.Constants.Contains (Key) then
               return Env.Constants.Element (Key);
            else
               raise Constraint_Error
                 with "Missing constant value: " & Key;
            end if;
         end if;
      end Translate_Key;

      Opt : constant String := Get_Attribute (Elem, L (Attr), Def);
   begin
      if Opt = "" then
         return Opt;
      elsif Opt (Opt'First) = '$' then
         if Contains (Opt, "=") then
            return Boolean'Image
              (Translate_Key (L (Select_Field (Opt, 1, '='))) =
                 L (Select_Field (Opt, 2, '=')));
         else
            return Translate_Key (L (Opt));
         end if;
      else
         return Opt;
      end if;
   end Translate_Attribute;

   ------------
   -- Option --
   ------------

   function Option (This : Object;
                    Attr : Option_Attr;
                    Def  : String) return String
   is
   begin
      return Translate_Attribute
        (This.Config, String (Attr), Def, This.Env.Id, This.Env);
   end Option;

   ------------
   -- Option --
   ------------

   function Option (This : Object;
                    Attr : Option_Attr) return String is
   begin
      This.Verify (Attr);
      return This.Option (Attr, "");
   end Option;

   ------------
   -- Exists --
   ------------

   function Exists (This : Object;
                    Attr : Option_Attr) return Boolean
   is
   begin
      return This.Option (Attr, "") /= "";
   end Exists;

   ------------
   -- Verify --
   ------------

   procedure Verify (This : Object; Attr : Option_Attr) is
   begin
      if not This.Exists (Attr) then
         raise Constraint_Error with "Option missing: " & String (Attr);
      end if;
   end Verify;

   ----------------
   -- Get_Config --
   ----------------

   function Get_Config (This : Object'Class) return Agpl.Xml.Node is
   begin
      return This.Config;
   end Get_Config;

   ----------------
   -- Get_Option --
   ----------------

   function Get_Option (This : Object'Class;
                        Attr : Option_Attr;
                        Def  : Xt) return Xt
   is
--        function Get is new Agpl.Xml.Get_Generic_Value
--          (Object'Class,
--           Option_Attr,
--           Xt,
--           Get_Config,
--           Image,
--           Image,
--           Value);
   begin
      return Value (This.Option (Attr, Image (Def)));
--        return Get (This, Attr, Def);
   end Get_Option;

   ---------------------------
   -- Print_Correspondences --
   ---------------------------

   procedure Print_Correspondences (This : Object) is
      procedure Printie (I : I_E_Maps.Cursor) is
         use I_E_Maps;
      begin
         Log (String (Key (I)) & " --> " & String (Element (I)), Always);
      end Printie;
      procedure Printei (I : E_I_Maps.Cursor) is
         use E_I_Maps;
      begin
         Log (String (Key (I)) & " --> " & String (Element (I)), Always);
      end Printei;
   begin
      Log ("INTERNAL --> EXTERNAL", always);
      This.I_E_Keys.Iterate (Printie'Access);
      Log ("EXTERNAL --> INTERNAL", always);
      This.E_I_Keys.Iterate (Printei'Access);
   end Print_Correspondences;

   --------------
   -- Provided --
   --------------

   function Provided (This : Object;
                      Key  : Internal_Key) return Boolean is
   begin
      return This.I_E_Keys.Contains (Key);
   end Provided;

   -------------------------
   -- Provided_And_Exists --
   -------------------------

   function Provided_And_Exists (This : Object;
                                 Key  : Internal_Key) return Boolean is
   begin
      return This.Provided (Key) and then This.Exists (Key);
   end Provided_And_Exists;

end Sancta.Component.Root;
