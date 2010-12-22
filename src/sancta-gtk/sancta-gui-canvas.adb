package body Sancta.Gui.Canvas is

   ------------
   -- Create --
   ------------

   function Create
     (Name : in String;
      Area : in Gtk.Widget.Gtk_Widget)
      return Object
   is
   begin
      return
        (Name => +Name,
         Area => Area);
   end Create;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (This : in Object) return String is
   begin
      return +This.Name;
   end Get_Name;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (This : in Object) return Gtk.Widget.Gtk_Widget is
   begin
      return This.Area;
   end Get_Widget;

end Sancta.Gui.Canvas;
