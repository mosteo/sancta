with Gtkada.Intl; use Gtkada.Intl;

package body Expres_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Expres", Msg);
   end "-";

end Expres_Intl;
