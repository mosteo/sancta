with Agpl.Containers.String_String_Maps;
with Agpl.Containers.String_Vectors;
with Agpl.Monitor;
with Agpl.Xml;
with Agpl; use Agpl;

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

with Gnat.MD5;

with Sancta.Component;

--  ON PLUG-IN ORDERING:
--  First, groups within an agent are added, in order of appearnace,
--     components within each group added in order of appearance.
--  Finally, stand-alone elements in the agent are added, in order of appearance.
--  This list, thus built, of components, is revised. Any component without
--    order receives a consecutive ordering, starting at ~6000.
--  Once all components have order, they're created.

package Sancta.Config is

   pragma Limitation ("This is a singleton. Only one config per executable.");
   --  Not sure there's need for more however...
   --  Well, there could be if we wanted to simulate several from a single exe.

   pragma Limitation ("No group or agent may share name");

   --  pragma Elaborate_Body;

   Log_Section : constant String := "sancta.config";

   function Get_Hash return Gnat.MD5.Message_Digest;
   --  MD5 of the configuration XML

   procedure Init (Id   :        Node_Id;
                   File :        String);
   --  Read config from file.

   procedure Init_Str (Id   :        Node_Id;
                       Conf :        String);
   --  Pass config in string.

   procedure Create_Plugin (Name :        String;
                            Conf :        Xml.Node);

   procedure Create_Plugins;

   function Get_Id return Node_Id;
   --  Node id for the current executable.

   function Get_All_Options return Xml.Document;
   --  Root element, with all options

   function Options return Xml.Node;
   --  Return the element with id = Node_Id

   function Named_Options (Elem : String) return Xml.Node;
   --  Return options for some other element but ours.

   procedure Enable_Log_Sections;
   --  This is done as part of initialization anyway

   procedure Set_Log_Options;
   --  This is done as part of initialization anyway

private

   package Plugin_Vectors is new Ada.Containers.Vectors
     (Positive, Component.Object_Access, Component."=");

   package Node_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Xml.Node, "<", Xml."=");

--     package Plugin_Maps is new Ada.Containers.Indefinite_Ordered_Maps
--       (String, Component.Object_Access, "<", Component."=");

   Id              : Node_Id := No_Node;

   Groups          : Agpl.Containers.String_Vectors.Vector;
   --  All group this node belongs to

   All_Options     : Xml.Node;
   Options_Map     : Node_Maps.Map;

   Constants       : Agpl.Containers.String_String_Maps.Map;

   Hash            : Gnat.Md5.Message_Digest;

   Mutex           : aliased Agpl.Monitor.Counting_Semaphore;
   --  Used to ensure that the global environment is not touched
   --    between setting and plugin creation.
   pragma Shame_On_Me;

   procedure Read_Constants;
   procedure Read_Options;

end Sancta.Config;
