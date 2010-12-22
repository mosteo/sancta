with Sancta.Component.Environment;
with Sancta.Component.Factory;
with Sancta.Component.Helper;
with Sancta.Component.Root;
with Sancta.Events;
with Sancta.Options; use Sancta.Options;

with Agpl.Chronos;
with Agpl.Command_Line;
with Agpl.Calendar.Format;
with Agpl.Os_Utils;
with Agpl.Reflection;
with Agpl.Strings;
with Agpl.Strings.Fields;
with Agpl.Trace.File;
with Agpl.Trace.Utils;

package body Sancta.Config is

   use type Xml.Node;

   ----------
   -- Hash --
   ----------

   function Get_Hash return Gnat.MD5.message_Digest is
   begin
      return Hash;
   end Get_Hash;

   -----------------
   -- Init_Common --
   -----------------

   procedure Init_Common is
      Logger : constant Agpl.Trace.File.Object_Access :=
                 new Agpl.Trace.File.Object;
   begin
      Hash := Gnat.Md5.Digest (Xml.To_String (All_Options));

      --  Check space for log:
      if Agpl.Os_Utils.Get_Free_Disk_Space (Agpl.Command_Line.Program_Path) <
        1024.0 * 1024.0 * 1024.0
      then
         raise Program_Error
           with "Insufficient free disk for safe logging:" &
             Agpl.Os_Utils.Get_Free_Disk_Space (Agpl.Command_Line.Program_Path)'Img;
      end if;

      --  Set config elements
      declare
         use type Xml.Node_Array;
         Nodes : constant Xml.Node_Array :=
                   Xml.Get_All ("group", All_Options) &
                   Xml.Get_All ("agent", All_Options);
      begin
         for I in Nodes'Range loop
            Options_Map.Insert (Xml.Get_Attribute (Nodes (I), "id"),
                                     Nodes (I));
         end loop;
      end;

      --  Detect groups
      declare
         Groups : constant Xml.Node_Array :=
                    Xml.Get_All ("group", Options);
      begin
         for I in Groups'Range loop
            if not Config.Groups.Contains (Xml.Get_Value (Groups (I))) then
               Config.Groups.Append (Xml.Get_Value (Groups (I)));
            else
               raise Constraint_Error
                 with "Duplicated group: " & Xml.Get_Value (Groups (I));
            end if;
         end loop;
      end;

      --  Mandatory logging to disk:
      Logger.Set_File
        (Command_Line.Program_Name & "." &
         Image (Id) & "." &
         Calendar.Format.Datestamp (Separator => '.') & "." &
         Calendar.Format.Timestamp & ".log");
      Agpl.Trace.Add_Tracer (Trace.Object_Access (Logger));

      Set_Log_Options;
      Enable_Log_Sections;
      Read_Options;   -- Global SANCTA settings
      Read_Constants; -- Configuration constants

   end Init_Common;

   ----------
   -- Init --
   ----------

   procedure Init (Id   :        Node_Id;
                   File :        String)
   is
      use Agpl.Command_Line;
   begin
      Config.Id := Id;

      All_Options := Xml.Parse (File);

      Init_Common;
   end Init;

   --------------
   -- Init_Str --
   --------------

   procedure Init_Str (Id   :        Node_Id;
                       Conf :        String)
   is
   begin
      Config.Id   := Id;

      All_Options := Xml.From_String (Conf);

      Init_Common;
   end Init_Str;

   -------------------
   -- Create_Plugin --
   -------------------

   procedure Create_Plugin (Name :        String;
                            Conf :        Xml.Node)
   is
      Timer : Agpl.Chronos.Object; -- To measure creation time

      Plugin_Name : String renames Name;
      New_Plugin  : Component.Object_Access;
      Env         : constant Component.Environment.Object :=
                      (Id        => Id,
                       Constants => Constants);
      Enabled     : constant Boolean := Boolean'Value
        (Component.Root.Translate_Attribute
           (Conf, "enabled", "true", Id, Env));

      M : Agpl.Monitor.Object (Mutex'Access); pragma Unreferenced (M);
   begin
      if Enabled then
         Component.Root.Set_Environment (Env);
         Log ("Component [" & Plugin_Name & "] being created...",
              Informative, Log_Section);

         select
            delay Max_Component_Creation_Time.Value;
            raise Program_Error
              with "Create didn't return for [" & Plugin_Name & "]";

         then abort
            New_Plugin := Component.Factory.Create (Plugin_Name, Conf, Env);

            if New_Plugin.Is_Master then
                Log ("Component [master:" & Plugin_Name & "] being launched...",
                    Debug, Log_Section);
            else
               Log ("Component [" & Plugin_Name & "] being launched...",
                    Debug, Log_Section);
            end if;

            Events.Start_Plugin (New_Plugin);

            if New_Plugin.Is_Master then
               Log ("Component [master:" & Plugin_Name & "] running after " &
                    Timer.Image,
                    Informative, Log_Section);
            else
               Log ("Component [" & Plugin_Name & "] running after " &
                    Timer.image,
                    Informative, Log_Section);
            end if;
         end select;
      else
         Log ("Skipping disabled plugin [" & Plugin_Name & "]",
              Debug, Log_Section);
      end if;
   exception
      when E : others =>
         Log ("While creating [" & Plugin_Name & "]: " & Report (E),
              Error);
         raise;
   end Create_Plugin;

   --------------------
   -- Create_Plugins --
   --------------------

   procedure Create_Plugins is
      use Xml.Node_Vectors;
      pragma Assert (Options /= null);
      Plugs : Vector;

      function Order (L, R : Xml.Node) return Boolean is
      begin
         return
           Integer'Value (Xml.Get_Attribute (L, "order", Integer'Last'Img)) <
           Integer'Value (Xml.Get_Attribute (R, "order", Integer'Last'Img));
      end Order;

      package Order_Sort is new Xml.Node_Vectors.Generic_Sorting (Order);

      procedure Add (I : Agpl.Containers.String_Vectors.Cursor) is
         Local : constant Xml.Node_Array :=
                   Xml.Get_All
                     ("component", Named_Options
                        (Agpl.Containers.String_Vectors.Element (I)));
      begin
         for J in Local'Range loop
            Plugs.Append (Local (J));
         end loop;
      end Add;

      Missing_Order : Natural := 6976;

      procedure Default_Order (I : Cursor) is
         Node : constant Xml.Node := Element (I);
      begin
         Xml.Set_Attribute
           (Node, "order",
            Xml.Get_Attribute (Node, "order", Missing_Order'Img));
         Missing_Order := Missing_Order + 1;
      end Default_Order;

      ---------------------------
      -- Correspondences_Check --
      ---------------------------

      procedure Correspondences_Check is
      --  Check them using fake helper components.
      --  This way we can report before any plugin creation,
      --    and the plugins can be created concurrently or by dependencies
      begin
         Component.Root.Clear_Node_Correspondences;
         for I in Plugs.First_Index .. Plugs.Last_Index loop
            declare
               pragma Warnings (Off);
               Help : constant Component.Helper.Object :=
                 Component.Helper.Create (Plugs.Element (I));
               pragma Warnings (On);
            begin
               null;
            end;
         end loop;
         Component.Root.Check_Node_Correspondences;
      end Correspondences_Check;

   begin
      Groups.Iterate (Add'Access);
      Plugs.Append (Xml.Get_All ("component", Options));

      --  Prepare appearance order for missing order clauses
      Plugs.Iterate (Default_Order'Access);

      Order_Sort.Sort (Plugs);

      Correspondences_Check;

      Log ("SANCTA component creation schedule:", Informative, Log_Section);
      for I in Plugs.First_Index .. Plugs.Last_Index loop
         declare
            use Agpl.Strings;
            Name : constant String :=
                     Xml.Get_Attribute (Plugs.Element (I), "name", "");
            Env         : constant Component.Environment.Object :=
                            (Id        => Id,
                             Constants => Constants);
            Enabled     : constant Boolean := Boolean'Value
              (Component.Root.Translate_Attribute
                 (Plugs.Element (I), "enabled", "true", Id, Env));
         begin
            if Enabled then
               Log (Rpad (To_String (I), 3) & ":" & " --> " & Name,
                    Informative, Log_Section);
            else
               Log (Rpad (To_String (I), 3) & ":" & "     " & Name,
                    Informative, Log_Section);
            end if;
         end;
      end loop;

      Log ("SANCTA component creation starting...", Informative, Log_Section);

      for I in Plugs.First_Index .. Plugs.Last_Index loop
         declare
            Plugin_Name : constant String :=
              Xml.Get_Attribute (Plugs.Element (I), "name", "");
         begin
            Create_Plugin (Plugin_Name, Plugs.Element (I));
         exception
            when E : others =>
               Log ("While creating [" & Plugin_Name & "]: " & Report (E),
                    Error);
               raise;
         end;
      end loop;

      Events.Start_Events;

   end Create_Plugins;

   --------
   -- Id --
   --------

   function Get_Id return Node_Id is
   begin
      return Id;
   end Get_Id;

   -------------
   -- Options --
   -------------

   function Options return Xml.Document is
   begin
      return Named_Options (Image (Id));
   end Options;

   ---------------------
   -- Set_Log_Options --
   ---------------------

   procedure Set_Log_Options is
   begin
      Agpl.Trace.Set_Decorator
        (Agpl.Trace.Utils.Prepend_Level_Timestamp_Section'Access);

      if Xml.Get_Attribute (Options, "log_level", "") = "" then
         Agpl.Trace.Set_Level (Agpl.Trace.Informative);
         Log ("No log_level given for agent, using Informative", Informative);
      else
         Agpl.Trace.Set_Level
           (All_Levels'Value (Xml.Get_Attribute (Options, "log_level")));
      end if;
   end Set_Log_Options;

   -------------------------
   -- Enable_Log_Sections --
   -------------------------

   procedure Enable_Log_Sections is
      use Agpl.Containers.String_Vectors;
      procedure Add (S : String) is
         Sections : constant Xml.Node_Array :=
                      Xml.Get_All ("log", Named_Options (S));
      begin
         for I in Sections'Range loop
--              Put_Line ("Enabling logging for [" &
--                        Xml.Get_Value (Sections (I)) & "]");
            if Xml.Get_value (Sections (I), "x") /= "x" then
               Enable_Section (Xml.Get_Value (Sections (I)));
            end if;
            if Xml.Get_attribute (Sections (I), "section", "") /= "" and then
              Xml.Get_attribute (Sections (I), "level", "") /= ""
            then
               Enable_Section
                 (Xml.Get_attribute (Sections (I), "section"),
                  Trace.All_Levels'Value (Xml.Get_Attribute (Sections (I), "level")));
            end if;
         end loop;
      end Add;
      procedure Add (I : Cursor) is
      begin
         Add (Element (I));
      end Add;
   begin
      Add (Image (Id));
      Groups.Iterate (Add'Access);
   end Enable_Log_Sections;

   -----------------
   -- All_Options --
   -----------------

   function Get_All_Options return Xml.Document is
   begin
      return All_Options;
   end Get_All_Options;

   -------------------
   -- Named_Options --
   -------------------

   function Named_Options (Elem : String) return Xml.Node
   is
   begin
      return Options_Map.Element (Elem);
   exception
      when others =>
         Log ("Couldn't find named options for: " & Elem, Warning, Log_Section);
         raise;
   end Named_Options;

   ------------------
   -- Read_Options --
   ------------------

   procedure Read_Options is
      Opts : constant Xml.Node_Vector :=
        Xml.Get_All (All_Options, "global");
   begin
      for I in Opts.First_Index .. Opts.Last_Index loop
         declare
            Name   : constant String :=
                       Xml.Get_Attribute (Opts.Element (I), "name");
            Value  : constant String :=
                       Xml.Get_Attribute (Opts.Element (I), "value", "");
            Option : constant String :=
                       Xml.Get_Attribute (Opts.Element (I), "option", "");
         begin
            if Value /= "" and then Option /= "" then
               raise Constraint_Error with "Both value and option given";
            end if;

            if Value /= "" then
               Agpl.Reflection.Set (Name, Value);
               Log ("Option set [" & Name & "=" & Value & "]",
                    Debug, Log_Section);
            else
               Agpl.Reflection.Set
                 (Name, Agpl.Command_Line.Get_Option (Option));
               Log ("Option set to c-line value [" & Name & "=" &
                    Agpl.Reflection.Get (Name) & "]",
                    Debug, Log_Section);
            end if;
         end;
      end loop;
   end Read_Options;

   --------------------
   -- Read_Constants --
   --------------------

   procedure Read_Constants is
      use Agpl.Strings.Fields;
      Cons : constant Xml.Node_Vector :=
               Xml.Get_All (All_Options, "constant");
   begin
--        Log ("CONSTANTS: " & Cons.Length'Img, Always);
      for I in Cons.First_Index .. Cons.Last_Index loop
         declare
            Name   : constant String :=
                       Xml.Get_Attribute (Cons.Element (I), "name");
            Value  : constant String :=
                       Xml.Get_Attribute (Cons.Element (I), "value", "");
            Option : constant String :=
                       Xml.Get_Attribute (Cons.Element (I), "option", "");
            Values : constant String :=
                       Xml.Get_Attribute (Cons.Element (I), "values", "");
         begin
            if Name (Name'First) /= '$' then
               raise Constraint_Error
                 with "Constant name must start with $: " & Name;
            else

               if Value /= "" and then Option /= "" then
                  raise Constraint_Error with "Both value and option given";
               end if;

               if Value /= "" then
                  if Values /= "" and then not Contains_Word (Values, Value) then
                     raise Constraint_Error with
                       "Value [" & Value &
                     "] for constant [" & Name &
                     "] not in values [" & Values & "]";
                  end if;

                  Constants.Insert (Name, Value);
                  Log ("Added inline constant [" & Name & "=" & Value & "]",
                       Debug, Log_Section);
               else
                  if Values /= "" and then
                    not Contains_Word (Values, Agpl.Command_Line.Get_Option (Option))
                  then
                     raise Constraint_Error with
                       "Value [" & Agpl.Command_Line.Get_Option (Option) &
                     "] for constant [" & Name &
                     "] not in values [" & Values & "]";
                  end if;

                  Constants.Insert
                    (Name, Agpl.Command_Line.Get_Option (Option));
                  Log ("Added c-line constant [" & Name & "=" &
                       Constants.Element (Name) & "]",
                       Debug, Log_Section);
               end if;
            end if;
         end;
      end loop;
   end Read_Constants;

end Sancta.Config;
