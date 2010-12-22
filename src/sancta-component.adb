package body Sancta.Component is

   -----------
   -- Image --
   -----------

   function Image (X : Option_Attr) return String is
   begin
      return String (X);
   end Image;

   ------------
   -- Equals --
   ------------

   function Equals (L, R : Data'Class) return Boolean is
      use type Data'Class;
   begin
      return L = R;
   end Equals;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (X : Internal_Key_Array) return Internal_Key_Vector is
      V : Internal_Key_Vector;
   begin
      for I in X'Range loop
         V.Append (X (I).all);
      end loop;

      return V;
   end To_Vector;

   ---------------
   -- Is_Master --
   ---------------

   function Is_Master (This : Object) return Boolean is
      pragma Unreferenced (This);
   begin
      return False;
   end Is_Master;

   ----------
   -- Wrap --
   ----------

   function Wrap (Datum : Data'Class;
                  Label : String;
                  Owner : Node_Id) return Data_Parcel
   is
   begin
      return (Owner => Owner,
              Label => +Label,
              Datum => Data_Handles.Set (Datum));
   end Wrap;

end Sancta.Component;
