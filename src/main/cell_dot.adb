with Sancta.Tasks.Grid_Goal;
with Sancta.Types;
use Sancta;

with Agpl.Containers.String_Sets;
with Agpl.Conversions; use Agpl.Conversions;
pragma Warnings (Off); with Agpl.Task_Termination; pragma Warnings (On);
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

procedure Cell_dot is

   package Grid renames Tasks.Grid_Goal;

   procedure Usage is
   begin
      Put_Line ("<floorplan>");
   end Usage;

   function S is new Conversions.To_Str (Types.Real);

begin
   if Argument_Count /= 1 then
      Usage;
      return;
   end if;

   Grid.Parse_Ascii (Argument (1));

   declare
      Costs : constant Grid.Grid_Graphs.Cost_Matrix :=
                Grid.Graph.Get_Costs;
      use Containers.String_Sets;
      Nodes : Set;

      procedure Add_Node (I : Grid.Grid_Graphs.Vertex_Index) is
         Name : constant String := "c" & To_String (Integer (I));
         Pose : constant Types.Pose :=
                  Grid.Graph.Get_Vertex (I).Data;
      begin
         if not Nodes.Contains (Name) then
            Nodes.Insert (Name);

            Put_Line ("node [pos=""" & S (Pose.X, 0) & "," & S(Pose.Y, 0) & "!"",shape=point,label="""",width=0.5,height=0.5] " &
                      Name & "; ");
         end if;
      end Add_Node;
   begin
      Put_Line ("Graph G {");
      Put_Line ("landscape=true");
      Put_Line ("center=true");
      --  Put_Line ("normalize=true");
      --  Put_Line ("damping=0.0");
      --  Put_Line ("maxiter=0");
      for R in Costs.First_Row .. Costs.Last_Row loop
         for C in Costs.First_Col .. Costs.Last_Col loop
            if Integer (R) < Integer (C) then
               if Costs.Get (R, C) = 1 then
                  Add_Node (R);
                  Add_Node (C);
                  Put_Line ("c" &
                            To_String (Integer (R))
                            & "--" &
                            "c" & To_String (Integer (C)));
               end if;
            end if;
         end loop;
      end loop;
      Put_Line ("}");
   end;

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Report (E));
end Cell_Dot;
