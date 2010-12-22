with Gtkada.Intl; use Gtkada.Intl;

package body Visor_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Visor", Msg);
   end "-";

end Visor_Intl;
