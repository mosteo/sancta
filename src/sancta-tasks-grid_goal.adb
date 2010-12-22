with Sancta.Agent_Proxy;
with Sancta.Debug;
with Sancta.Types.Operations;

with Agpl.Containers.String_Sets;
with Agpl.Conversions; use Agpl.Conversions;
with Agpl.Filesystem;
with Agpl.Generic_Handle;
with Sancta.Tasks.Utils;
with Agpl.Random;

with Ada.Text_Io;

with Gnat.Os_Lib;

package body Sancta.Tasks.Grid_Goal is

   package Agent_Lists renames Sancta.Agent.Containers.Lists;

   package Cost_Handles is
     new Agpl.Generic_Handle (Grid_Graphs.Cost_Matrix, Grid_Graphs."=");
   use Cost_Handles;
   use type Types.Real;

   function S is new Conversions.To_Str (Types.Real);

   Cost : Cost_Handles.Object;

   ------------
   -- Create --
   ------------

   function Create (Cell : in Grid_Graphs.Vertex_Index)
                    return    Object
   is
   begin
      return (Sancta.Tasks.Primitive.Object with
              Cell => Cell);
   end Create;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost (From,
                      To      : in Grid_Graphs.Vertex_Index)
                      return       Sancta.Costs
   is
   begin
      if not Cost.Is_Valid then
         Cost.Set (Grid_Graphs.Get_Costs (Graph));
      end if;

      declare
         C     : constant Sancta.Costs := Sancta.Costs (Cost.Ref.Get (From, To));
         Noise :          Sancta.Costs := 0.0;
         use type Sancta.Costs;
      begin
         if C >= Sancta.Costs (1_000_000_000) then
            return Sancta.Infinite;
         else
            if Noisify then
               Noise := Sancta.Costs ((Random.Uniform - 0.5) / 100.0);
               pragma Assert (Noise >= -0.01 and then Noise <= 0.01);
            end if;
            return C * Cost_Adjust + Noise;
         end if;
      end;
   end Get_Cost;

   --------------
   -- Get_Cell --
   --------------

   function Get_Cell (This : in Object) return Grid_Graphs.Vertex_Index is
   begin
      return This.Cell;
   end Get_Cell;

   --------------
   -- Get_Cell --
   --------------

   function Get_Cell (This : in Types.Pose) return Grid_Graphs.Vertex_Index is
      use Tasks.Grid_Goal.Grid_Graphs;
      use Types.Operations;
      Vertices       : constant Vertex_Vectors.Vector :=
                         Tasks.Grid_Goal.Graph.Get_Vertices;
      Closest_Vertex : Vertex_Index := Vertex_Index'Last;
      Closest_Dist   : Types.Real   := Types.Real'Last;
   begin
      for I in Vertices.First_Index .. Vertices.Last_Index loop
         if Distance (Get_Pose (I), This) < Closest_Dist then
            Closest_Dist   := Distance (Get_Pose (I), This);
            Closest_Vertex := I;
         end if;
      end loop;
      if Closest_Vertex = Vertex_Index'Last then
         raise Constraint_Error with "No close vertex found!?";
      else
         return Closest_Vertex;
      end if;
   end Get_Cell;

   --------------
   -- Get_Pose --
   --------------

   function Get_Pose (V : in Grid_Graphs.Vertex_Index) return Types.Pose
   is
   begin
      return Graph.Get_Vertex (V).Data;
   end Get_Pose;

   --------------
   -- Get_Pose --
   --------------

   function Get_Pose (This : in Object) return Types.Pose is
   begin
      return Get_Pose (This.Cell);
   end Get_Pose;

   -----------------
   -- Parse_Ascii --
   -----------------

   procedure Parse_Ascii (File : in String; Add_Wall_Goals : Boolean := True) is
      use Ada.Text_Io;
      F : File_Type;

      Wall : constant Character := '*';
      Goal : constant Character := 'x';
      Free : constant Character := ' ';
      Bot  : constant Character := 'r';

      function Is_Free (C : in Character) return Boolean is
      begin
         return C = Free or else C = Bot or else C = Goal;
      end Is_Free;

      type Cells is record
         Char : Character := Wall;
         Cell : Grid_Graphs.Vertex_Index;
      end record;
      pragma Pack (Cells);
   begin
      Open (F, In_File, File);
      declare
         First_Line : constant String := Get_Line (F);
         Map        : array (1 .. First_Line'Length, 1 .. First_Line'Length)
           of Cells;
         Row        : Positive := 1;
         use Types;
         use type Grid_Graphs.Vertex_Index;
      begin
         Close (F);
         Open (F, In_File, File);

         --  Read vertices
         loop
            exit when End_Of_File (F);
            declare
               Line : constant String := Get_Line (F);
            begin
               pragma Assert (Line'First = Map'First);
               for Col in Line'Range loop

                  --  Add vertex for empty space
                  if Is_Free (Line (Col)) then
                     Graph.Add_Vertex
                       ((Graph.Max_Vertex + 1,
                         (Real (Col), Real (Map'Last - Row), 0.0)));
                  end if;

                  Map (Row, Col) := (Line (Col), Graph.Max_Vertex);

                  if Line (Col) = Goal then
                     --  Add goal task
                     Mission.Append (Create (Graph.Max_Vertex));
                  elsif Line (Col) = Bot then
                     --  Add robot
                     declare
                        Ag : Agent_Proxy.Object;
                     begin
                        Ag.Set_Name
                          (Image (Types.Agent_Names
                             (Character'Val
                                (Character'Pos ('a') +
                                 Natural (Agents.Length)))));
                        Ag.Set_Pose ((Real (Col), Real (Map'Last - Row), 0.0));
                        Agents.Append (Ag);
                     end;
                  end if;
               end loop;
               Row := Row + 1;
            end;
         end loop;

         --  Create edges (Assumes the bounding box is wall!!)
         for Row in Map'First (1) .. Map'Last (1) - 1 loop
            for Col in Map'First (2) .. Map'Last (2) - 1 loop
               if Is_Free (Map (Row, Col).Char) and then
                 Is_Free (Map (Row, Col + 1).Char) then
                  Graph.Add_Undirected_Edge
                    ((Map (Row, Col).Cell, Map (Row, Col + 1).Cell, Weight => 1));
               end if;
               if Is_Free (Map (Row, Col).Char) and then
                 Is_Free (Map (Row + 1, Col).Char) then
                  Graph.Add_Undirected_Edge
                    ((Map (Row, Col).Cell, Map (Row + 1, Col).Cell, Weight => 1));
               end if;
            end loop;
         end loop;

         --  Create goals adjacent to walls
         if Add_Wall_Goals then
            for Row in Map'First (1) + 1 .. Map'Last (1) - 1 loop
               for Col in Map'First (2) + 1 .. Map'Last (2) - 1 loop
                  if Map (Row, Col).Char = Free and then
                    (Map (Row, Col + 1).Char = Wall or else
                     Map (Row, Col - 1).Char = Wall or else
                     Map (Row + 1, Col).Char = Wall or else
                     Map (Row - 1, Col).Char = Wall)
                  then
                     Mission.Append (Create (Map (Row, Col).Cell));
                  end if;
               end loop;
            end loop;
         end if;

         Log ("Created" & Graph.Max_Vertex'Img & " vertices.",
              Informative, Log_Section);
         Log ("Created" & Graph.Get_Edges'Length'Img & " edges.",
              Informative, Log_Section);
         Log ("Created" & Mission.Length'Img & " tasks.",
              Informative, Log_Section);
         Log ("Created" & Agents.Length'Img & " agents.",
              Informative, Log_Section);

         --  Load costs
         declare
            Cost_File : String renames
              Filesystem.Replace_Extension (File, "gcosts");
         begin
            if Gnat.Os_Lib.Is_Readable_File (Cost_File) then
               Log ("Using costs file.", Informative, Log_Section);
               Cost.Set (Cost_Store.Load (Cost_File));
            else
               Graph.Compute_Costs;
            end if;
         end;
      end;
   exception
      when E : others =>
         Log ("Parsing " & File & ": " & Report (E), Error);
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end Parse_Ascii;

   -----------------------
   -- Create_From_Poses --
   -----------------------

   procedure Create_From_Poses (Poses         : in Types.Pose_Array;
                                Link_Distance : in Float := 5.0)
   is
      use type Grid_Graphs.Vertex_Index;
      use Types.Operations;
      Curr_Vertex : Grid_Graphs.Vertex_Index := Grid_Graphs.Vertex_Index'First;
   begin
      --  Add cells and tasks
      for I in Poses'Range loop
         Graph.Add_Vertex ((Curr_Vertex, Poses (I)));
         Mission.Append (Create (Curr_Vertex));
         Curr_Vertex := Curr_Vertex + 1;
      end loop;

      --  Create links. Graph can be split.
      declare
         Vertices : constant Grid_Graphs.Vertex_Vectors.Vector :=
                      Graph.Get_Vertices;
      begin
         for I in Vertices.First_Index .. Vertices.Last_Index - 1 loop
            for J in I + 1 .. Vertices.Last_Index loop
               declare
                  Dist : constant Float :=
                           Float
                             (Distance
                                (Vertices.Element (I).Data,
                                 Vertices.Element (J).Data));
               begin
                  if Dist <= Link_Distance then
                     Graph.Add_Undirected_Edge
                       ((Vertices.Element (I).Index,
                        Vertices.Element (J).Index,
                        Integer (Float'Ceiling (Dist))));
                  end if;
               end;
            end loop;
         end loop;
      end;
   end Create_From_Poses;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Object) return String is
      P1 : Types.Pose renames Graph.Get_Vertex (This.Cell).Data;
   begin
      return
        "Grid goal at " & Debug.To_String (P1) &
        " [cell" & This.Cell'Img & "]" &
        "[id" & This.Get_Id'Img & "]";
   end To_String;

   ------------
   -- To_Dot --
   ------------

   function To_Dot return Ustring is
      use Asu;
      use type Grid_Graphs.Vertex_Index;
      Result : Ustring;
      NL  : constant String  := (Character'Val (13), Character'Val (10));

      use Agpl.Containers.String_Sets;
      Nodes : Agpl.Containers.String_Sets.Set;

      procedure Add_Node (I : Grid_Graphs.Vertex_Index) is
         Name : constant String := "c" & To_String (Integer (I));
         Pose : constant Types.Pose := Graph.Get_Vertex (I).Data;
      begin
         if not Nodes.Contains (Name) then
            Nodes.Insert (Name);

            Append (Result,
                    "node [pos=""" & S (Pose.X, 0) & "," & S (Pose.Y, 0) &
                     "!"",shape=point,label="""",width=0.5,height=0.5] " &
                     Name & "; " & NL);
         end if;
      end Add_Node;

      Edges : constant Grid_Graphs.Edge_Array := Graph.Get_Edges;
   begin
      Append (Result, "Graph G { " & NL);
      Append (Result, "landscape=true; " & NL);
      Append (Result, "center=true; " & NL);
      Append (Result, "dpi=12.0; "& NL);
      --  Put_Line ("normalize=true");
      --  Put_Line ("damping=0.0");
      --  Put_Line ("maxiter=0");
      for E in Edges'Range loop
         if Edges (E).Source < Edges (E).Dest then
            Add_Node (Edges (E).Source);
            Add_Node (Edges (E).Dest);
            Append (Result,
                    "c" &
                    To_String (Integer (Edges (E).Source))
                    & "--" &
                    "c" & To_String (Integer (Edges (E).Dest)) & Nl);
         end if;
      end loop;
      Append (Result, "}");
      return Result;
   end To_Dot;

   ------------
   -- To_Dot --
   ------------

   function To_Dot (Ass : in Sancta.Assignment.Object) return Ustring is
      use Asu;
      use type Grid_Graphs.Vertex_Index;
      Result : Ustring;
      NL  : constant String  := (Character'Val (13), Character'Val (10));

      Colors : constant array (1 .. 4) of String (1 .. 7) :=
                 ("#ff0000", "#00ff00", "#0000ff", "#ff00ff");
      Styles : constant array (1 .. 4) of String (1 .. 6) :=
                 ("dotted", "dashed", "solid ", "dotted");
      Shapes : constant array (1 .. 4) of String (1 .. 11) :=
                 ("circle     ",
                  "triangle   ",
                  "diamond    ",
                  "invtriangle");

      Ag_Idx : Positive := Colors'First;

      procedure Add_Agent (I : Agent_Lists.Cursor) is
         use Sancta.Tasks.Containers;
         use Sancta.Tasks.Utils;
         Ag : constant Agent_Proxy.Object :=
                Agent_Proxy.Object (Agent_Lists.Element (I));
         Tasks : constant Vectors.Vector :=
                   To_Vector (Agent_Lists.Element (I).Get_Tasks);
      begin
         --  Create agent start
         Append (Result,
                 "node [pos=""" & S (Ag.Get_Pose.X, 0) & "," & S (Ag.Get_Pose.Y, 0) &
                 "!"",shape=" & Shapes (Ag_Idx) & ",label="""",width=0.5,height=0.5," &
                 "color=""" & Colors (Ag_Idx) & """] " &
                 Ag.Get_Name & "; " & Nl);

         --  Create nodes
         for I in Tasks.First_Index .. Tasks.Last_Index loop
            declare
               Pose : constant Types.Pose :=
                        Graph.Get_Vertex
                          (Object (Tasks.Element (I)).Cell).Data;
               Name : constant String :=
                        Ag.Get_Name & To_String (I);
            begin
               Append (Result,
                       "node [pos=""" & S (Pose.X, 0) & "," & S (Pose.Y, 0) &
                       "!"",shape=" & Shapes (Ag_Idx) & ",label="""",width=0.5,height=0.5," &
                       "color=""" & Colors (Ag_Idx) & """] " &
                       Name & "; " & Nl);
            end;
         end loop;

         --  Create edges
         if not Tasks.Is_Empty then
            Append (Result,
                    Ag.Get_Name &
                    "->" &
                    Ag.Get_Name & To_String (Integer (Tasks.First_Index)) &
                    " [color=""" & Colors (Ag_Idx) & """," &
                    "arrowhead=open] " & Nl);
         end if;

         for I in Tasks.First_Index .. Tasks.Last_Index - 1 loop
            Append (Result,
                    Ag.Get_Name & To_String (I) &
                    "->" &
                    Ag.Get_Name & To_String (Integer (I + 1)) &
                    " [color=""" & Colors (Ag_Idx) & """," &
                    "arrowhead=open] " & Nl);
         end loop;

         Ag_Idx := Ag_Idx + 1;
      end Add_Agent;
   begin
      Append (Result, "digraph G { " & Nl);
      Append (Result, "landscape=true; " & Nl);
      Append (Result, "center=true; " & Nl);
      Append (Result, "dpi=15.0; " & Nl);

      declare
         Agents : constant Agent_Lists.List := Ass.Get_Agents;
      begin
         Agents.Iterate (Add_Agent'Access);
      end;

      Append (Result, "}");
      return Result;
   end To_Dot;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Mission.Clear;
      Graph.Clear;
      Agents.Clear;
      Cost.Clear;
   end Clear;

end Sancta.Tasks.Grid_Goal;
