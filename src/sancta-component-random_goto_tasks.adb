with Ada.Numerics,
     Agpl.Random,
     Sancta.Component.Ctypes,
     Sancta.Component.Factory,
     Sancta.Containers,
     Sancta.Convert,
     Sancta.Tasks.Goto_Pose,
     Sancta.Types.Operations;

use Ada.Numerics,
    Sancta.Containers;

package body Sancta.Component.Random_Goto_Tasks is

   use type Types.Real;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Comp_Config)
      return Component.Object_Access
   is
      This : constant Root.Object_Access := new Object (Name'Access, Config);
      Rang : constant Float :=
               Float'Value
                 (This.Option (Option_Range, Float'Image (Default_Range)));
      Base : Types.Pose;

      Jobs : Tc.Lists.List;
   begin
      if This.Exists (Option_Origin) then
         Base := Convert.To_Pose (This.Option (Option_Origin));
      else
         Base := Default_Origin;
      end if;

      Agpl.Random.Reset
        (Integer'Value
           (This.Option
              (Option_Random_Seed, Integer'Image (Default_Random_Seed))));

      while Natural (Jobs.Length) <
        Natural'Value
          (This.Option (Option_Amount, Natural'Image (Default_Amount)))
      loop
         declare
            Target : constant Tasks.Goto_Pose.Object :=
                       Tasks.Goto_Pose.Create
                         ((Base.X +
                            Types.Real (Agpl.Random.Get_Float (-Rang, Rang)),
                           Base.Y +
                            Types.Real (Agpl.Random.Get_Float (-Rang, Rang)),
                           Types.To_Angle
                            (Types.Real
                               (Agpl.Random.Get_Float (0.0, 2.0 * Pi)))));
         begin
            if Types.Operations.Distance (Target.Pose, Base) <=
               Types.Real (Rang)
            then
               Jobs.Append (Target);
            end if;
         end;
      end loop;

      This.Output (Provides_Tasks, Ctypes.Task_List'(Tasks => Jobs));
      return Component.Object_Access (This);
   end Create;

end Sancta.Component.Random_Goto_Tasks;
