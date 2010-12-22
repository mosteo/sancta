with Sancta.Agent;
with Sancta.Tasks.Utils;

package body Sancta.Cost_Matrix.Utils is

   package Ac renames Sancta.Agent.Containers;
   package Tc renames Sancta.Tasks.Containers;

   ------------
   -- Create --
   ------------

   procedure Create (Dst : in out Cost_Matrix.Object;
                     Ass : in     Assignment.Object)
   is
      use Ac.Lists;
      use Sancta.Tasks.Utils;

      procedure Check_Agent (I : Sancta.Agent.Containers.Lists.Cursor) is
         Agent : constant Sancta.Agent.Object'Class := Element (I);
         V     : constant Tc.Vectors.Vector := To_Vector (Ass.Get_Tasks (Agent));
      begin
         --  Starting tasks
         if not V.Is_Empty then
            Set_Cost
              (Dst, Agent.Get_Name,
               Sancta.Tasks.No_Task,
               V.First_Element.Get_Id,
               Agent.Get_Cost (V.First_Element));
         end if;

         --  Used tasks
         for I in V.First_Index .. V.Last_Index - 1 loop
            Set_Cost
              (Dst, Agent.Get_Name,
               V.Element (I).Get_Id,
               V.Element (I + 1).Get_Id,
               Agent.Get_Cost
                 (V.Element (I),
                  V.Element (I + 1)));
         end loop;
      end Check_Agent;
   begin
      Ass.Get_Agents.Iterate (Check_Agent'Access);
   end Create;

   -----------
   -- Prune --
   -----------

   procedure Prune
     (Dst :    out Cost_Matrix.Object;
      Src : in     Cost_Matrix.Object;
      Ass : in     Assignment.Object)
   is
      use Ac.Lists;
      use Sancta.Tasks.Utils;

      procedure Check_Agent (I : Sancta.Agent.Containers.Lists.Cursor) is
         Agent : constant Sancta.Agent.Object'Class := Element (I);
         V     : constant Tc.Vectors.Vector := To_Vector (Ass.Get_Tasks (Agent));
      begin
         --  Starting tasks
         if not V.Is_Empty then
            Set_Cost
              (Dst, Agent.Get_Name,
               Sancta.Tasks.No_Task,
               V.First_Element.Get_Id,
               Get_Cost
                 (Object'Class (Src),
                  Agent.Get_Name,
                  Sancta.Tasks.No_Task,
                  V.First_Element.Get_Id));
         end if;
         --  Used tasks
         for I in V.First_Index .. V.Last_Index - 1 loop
            Set_Cost
              (Dst, Agent.Get_Name,
               V.Element (I).Get_Id,
               V.Element (I + 1).Get_Id,
               Get_Cost
                 (Object'Class (Src),
                  Agent.Get_Name,
                  V.Element (I).Get_Id,
                  V.Element (I + 1).Get_Id));
         end loop;
      end Check_Agent;
   begin
      Ass.Get_Agents.Iterate (Check_Agent'Access);
   end Prune;

end Sancta.Cost_Matrix.Utils;
