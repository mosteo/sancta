with Gtkada.Intl; use Gtkada.Intl;

package body Setpose_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Setpose", Msg);
   end "-";

end Setpose_Intl;
