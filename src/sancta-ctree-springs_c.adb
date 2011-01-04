with Agpl.Trace; use Agpl.Trace;

with Interfaces.C; use Interfaces;

package body Sancta.Ctree.Springs_C is

   pragma Linker_Options ("-lplayerc++");

   -------------
   -- Springs --
   -------------

   task body Springs is
      procedure Springstask (Num_Robots : Natural;
                             K          : C.Double;
                             Gv         : C.Double;
                             R          : C.Double;
                             Kv         : C.Double;
                             F          : C.Double);
      pragma Import(C, Springstask, "SpringsTask");
   begin
      Springstask (Config.Num_Robots,
                   C.Double (Config.K),
                   C.Double (Config.Goal_v),
                   C.Double (Config.Rest_Pos),
                   C.Double (Config.Kv),
                   C.Double (Config.F));
   exception
      when E : others =>
         Log ("Sancta.Ctree.Springs_C: " & Report (E), Error);
   end Springs;

end Sancta.Ctree.Springs_C;
