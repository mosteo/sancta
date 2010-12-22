--  Facilities to create a static Sancta node, which doesn't require an
--   xml config file...

with Agpl.Containers.String_String_Maps;

package Sancta.Config.Static is

--  Since for now Sancta is limited to one config per exe, this is also
--   a singleton.

   procedure Add_Constant (Name : String; Value : String);
   --  Constants are values set to be referenced in component options

   generic
      type Item is private;
      with function Image (I : Item) return String;
   procedure Add_Typed_Constant (Name : String; Value : Item);

   procedure Add_Global (Name : String; Value : String);
   --  Globals are vars that are under Agpl.Reflection, so they affect some
   --    global setting of Sancta or Agpl. Better not overuse them?

   generic
      type Item is private;
      with function Image (I : Item) return String;
   procedure Add_Typed_Global (Name : String; Value : Item);

   package Acssm renames Agpl.Containers.String_String_Maps;

   type Option_Map   is new Acssm.Map with null record;
   type Requires_Map is new Acssm.Map with null record;
   type Provides_Map is new Acssm.Map with null record;

   No_Options  : constant Option_Map;
   No_Requires : constant Requires_Map;
   No_Provides : constant Provides_Map;

   procedure Add_Component
     (Name : Sancta.Component.Component_Name;
      Opts : Option_Map'Class   := No_Options;
      Requ : Requires_Map'Class := No_Requires;
      Prov : Provides_Map'Class := No_Provides);
   --  This component is made know; but it is not yet assigned to any agent

   type Component_Vector is new Agpl.Containers.String_Vectors.Vector with null record;

   procedure Launch_Agent (Id        : Node_Id;
                           Log_Level : All_Levels;
                           Comps     : Component_Vector);
   --  Use this instead of the facilities in Sancta.Launcher

   function Get_Xml return String;
   --  Return the actual XML config to be used

private

   No_Options  : constant Option_Map   := (Acssm.Empty_Map with null record);
   No_Requires : constant Requires_Map := (Acssm.Empty_Map with null record);
   No_Provides : constant Provides_Map := (Acssm.Empty_Map with null record);

end Sancta.Config.Static;
