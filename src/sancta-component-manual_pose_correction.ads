with Sancta.Network.Consumer;
with Sancta.Network.Layer;
with Sancta.Component.Root;
with Sancta.Types.Transformations;

with Agpl.Xml;

--  Reads the "manual_pose" and uses it as a delta for the underlying poses.
--  The delta can be provided via "new_pose" input or "set_pose" message

package Sancta.Component.Manual_Pose_Correction is

   pragma Elaborate_Body;

   Name        : aliased constant String := "manual_pose_correction";
   Log_Section : constant String := "sancta.component.manual_pose_correction";

   Requires_Link    : constant Internal_Key   := "link";
   --  Network link for set_pose messages
   Requires_In_Pose : constant Internal_Key   := "in_pose";
   --  The pose to adjust
   Requires_New_Pose : constant Internal_Key  := "new_pose";
   --  The manually provided pose, which is optional...
   Requires_Inverse_Pose : constant Internal_Key := "inverse_pose";
   --  Some pose we want to apply the inverse transformation.
   --  e.g. a goal in the "new_pose" reference that we want in "in_pose" ref.

   Provides_Out_Pose  : constant Internal_Key := "out_pose";
   --  The corrected pose
   Provides_Out_Inverse_Pose : constant Internal_Key := "out_inverse_pose";

   Provides_Manual_Pose : constant Internal_Key := "manual_pose";
   --  Outputted when a pose correction is received via network

   Option_Channel : constant Option_Attr := "channel";
   --  Channel to listen to.

   procedure Register;

private

   type Object; -- is new Root.Object with private;

   function Create (Config : in Agpl.Xml.Node) return Object_Access;

   use Types.Transformations.Real_Transf;

   type Listener_Type (This : access Object) is new Network.Consumer.Object
   with null record;

   overriding
   procedure On_Reception (Thix : in out Listener_Type;
                           M    : in     Network.Message'Class;
                           Meta : in     Network.Message_Metadata);

   type Object is new Root.Object with
      record
         Transf      : Transformation := Identity;
         Inv_Transf  : Transformation := Identity;

         Link        : Network.Layer.Object_Access;
         Chan        : Network.Channel;
         Listener    : aliased Listener_Type (Object'Access);
      end record;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Manual_Pose_Correction;
