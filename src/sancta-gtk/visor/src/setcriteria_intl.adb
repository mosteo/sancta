with Gtkada.Intl; use Gtkada.Intl;

package body Setcriteria_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Setcriteria", Msg);
   end "-";

end Setcriteria_Intl;
