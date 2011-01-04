with Ada.Containers.Indefinite_Ordered_Maps;

package body Sancta.Ctree.Data_Source is

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Data_Sources, Object'Class);

   Sources : Maps.Map;

   --------------
   -- Register --
   --------------

   procedure Register (Src : Data_Sources; This : Object'Class) is
   begin
      Sources.Insert (Src, This);
   end Register;

   ---------
   -- Get --
   ---------

   function Get (Src : Data_Sources) return Object'Class is
   begin
      return Sources.Element (Src);
   end Get;

end Sancta.Ctree.Data_Source;
