package body Sancta.Component.Cast is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create_Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Pass :=
        Boolean'Value (This.Option (Opt_Passthrough, Def_Passthrough'Img));

      This.Subscribe (Requires_In);

      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
   begin
      if Value in Data_Parcel'Class then
         declare
            Casted : Data_Parcel'Class := Data_Parcel'Class (Value);
         begin
            if Casted.Datum.Ref.all in Required'Class then
               Casted.Datum.Set (Cast (Required (Casted.Datum.Ref.all)));
               This.Output (Provides_Out, Casted);
            elsif This.Pass then
               This.Output (Provides_Out, Value);
            else
               raise Constraint_Error with
               "non-matching input: " & External_Tag (Casted.Datum.Ref.all'Tag);
            end if;
         end;
      elsif Value in Required'Class then
         This.Output (Provides_Out, Cast (Required (Value)));
      elsif This.Pass then
         This.Output (Provides_Out, Value);
      else
         raise Constraint_Error with
         "non-matching input: " & External_Tag (Value'Tag);
      end if;
   end Key_Stored;

end Sancta.Component.Cast;
