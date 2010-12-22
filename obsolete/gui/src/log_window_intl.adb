with Gtkada.Intl; use Gtkada.Intl;

package body Log_Window_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Log_Window", Msg);
   end "-";

end Log_Window_Intl;
