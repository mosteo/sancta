with Agpl.Xml; use Agpl.Xml;
with Sancta.Starter;

package body Sancta.Config.Static is

   Doc : constant Agpl.Xml.Document :=
           From_String ("<sancta />");

   ------------------
   -- Add_Constant --
   ------------------

   procedure Add_Constant (Name : String; Value : String) is
      Child : constant Node := Create_Child (Doc, "constant");
   begin
      Set_Attribute (Child, "name", Name);
      Set_Attribute (Child, "value", Value);
      Add (Doc, Child);
   end Add_Constant;

   ------------------------
   -- Add_Typed_Constant --
   ------------------------

   procedure Add_Typed_Constant
     (Name : String;
      Value : Item)
   is
   begin
      Add_Constant (Name, Image (Value));
   end Add_Typed_Constant;

   ----------------
   -- Add_Global --
   ----------------

   procedure Add_Global (Name : String; Value : String) is
      Child : constant Node := Create_Child (Doc, "global");
   begin
      Set_Attribute (Child, "name", Name);
      Set_Attribute (Child, "value", Value);
      Add (Doc, Child);
   end Add_Global;

   ----------------------
   -- Add_Typed_Global --
   ----------------------

   procedure Add_Typed_Global
     (Name : String;
      Value : Item)
   is
   begin
      Add_Global (Name, Image (Value));
   end Add_Typed_Global;

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component
       (Name : Sancta.Component.Component_Name;
        Opts : Option_Map'Class   := No_Options;
        Requ : Requires_Map'Class := No_Requires;
        Prov : Provides_Map'Class := No_Provides)
   is
      Comp : constant Node := Create_Child (Doc, "component");

      use Agpl.Containers.String_String_Maps;

      procedure Add_Option (I : Cursor) is
      begin
         Set_Attribute (Comp, Key (I), Element (I));
      end Add_Option;

      procedure Add_Requ (I : Cursor) is
         C : constant Node := Create_Child (Comp, "requires");
      begin
         Add (Comp, C);
         Set_Attribute (C, "data", Key (I));
         Set_Attribute (C, "as",   Element (I));
      end Add_Requ;

      procedure Add_Prov (I : Cursor) is
         C : constant Node := Create_Child (Comp, "provides");
      begin
         Add (Comp, C);
         Set_Attribute (C, "data", Key (I));
         Set_Attribute (C, "as",   Element (I));
      end Add_Prov;

   begin
      Add (Doc, Comp);
      Set_Attribute (Comp, "name", Name);
      Opts.Iterate (Add_Option'Access);
      Requ.Iterate (Add_Requ'Access);
      Prov.Iterate (Add_Prov'Access);
   end Add_Component;

   ------------------
   -- Launch_Agent --
   ------------------

   procedure Launch_Agent
     (Id        : Node_Id;
      Log_Level : All_Levels;
      Comps     : Component_Vector)
   is
      N : constant Node := Create_Child (Doc, "agent");
   begin
      Add (Doc, N);

      Set_Attribute (N, "id", Image (Id));
      Set_Attribute (N, "log_level", Log_Level'Img);

      for I in Comps.First_Index .. Comps.Last_Index loop
         Add (N,
              Get_With_Attribute (Doc, "component", "name", Comps.Element (I)));
      end loop;

      Starter.Launch (Id, To_String (Doc));
   end Launch_Agent;

   function Get_Xml return String is
   begin
      return To_String (Doc);
   end Get_Xml;

end Sancta.Config.Static;
