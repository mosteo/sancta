--  This object tracks the status of given tasks, without taking into
--  consideration HTN related aspects like expansion, etc.

with Sancta.Tasks;
with Sancta.Tasks.Choose_Entry_Point;
with Sancta.Tasks.Explore_Segment,
     Sancta.Tasks.Explore_Directed_Segment;
with Sancta.Tasks.Goto_Pose;
with Sancta.Tasks.Speed_Driving;
with Sancta.Types;
with Sancta.Convert;

with Sancta.Plan_Node;
with Agpl.Xml;
with Agpl; use Agpl;

with Ada.Text_Io; use Ada.Text_Io;

with Gnat.Os_Lib;

package body Sancta.Mission_Tracker is

   use Sancta.Tasks;

   type Known_Kinds is (Choose_Entry_Point,
                        Explore_Segment,
                        Explore_Directed_Segment,
                        Goto_Pose,
                        Speed_Driving);

   type Parser is access function (Job : in Xml.Node) return Sancta.Plan.Subplan;

   -----------------------
   -- Parse_Entry_Point --
   -----------------------

   function Parse_Entry_Point (Job : in Xml.Node) return Sancta.Plan.Subplan is
      Candidates : Types.Pose_Vector.Object (First => 1);
      Children   : constant Xml.Node_Array := Xml.Get_All (Job, "entry_point");
   begin
      for I in Children'Range loop
         Candidates.Append
           (Convert.To_Pose (Xml.Get_Attribute (Children (I), "pose", "")));
      end loop;
      declare
         Cand_Array : Types.Pose_Array (1 .. Candidates.Last);
      begin
         for I in Cand_Array'Range loop
            Cand_Array (I) := Candidates.Vector (I);
         end loop;
         return Sancta.Plan_Node.Create
           (Tasks.Choose_Entry_Point.Create (Cand_Array));
      end;
   end Parse_Entry_Point;

   ---------------------------
   -- Parse_Explore_Segment --
   ---------------------------

   function Parse_Explore_Segment (Job : in Xml.Node) return Sancta.Plan.Subplan is
   begin
      return Sancta.Plan_Node.Create
        (Tasks.Explore_Segment.Create
           (Convert.To_Pose (Xml.Get_Attribute (Job, "ini", "")),
            Convert.To_Pose (Xml.Get_Attribute (Job, "fin", ""))));
   end Parse_Explore_Segment;

   ----------------------------
   -- Parse_Explore_directed --
   ----------------------------

   function Parse_Explore_directed (Job : in Xml.Node) return Sancta.Plan.Subplan is
   begin
      return Sancta.Plan_Node.Create
        (Tasks.Explore_Directed_Segment.Create
           (Convert.To_Pose (Xml.Get_Attribute (Job, "ini", "")),
            Convert.To_Pose (Xml.Get_Attribute (Job, "fin", ""))));
   end Parse_Explore_directed;

   ---------------------
   -- Parse_Goto_Pose --
   ---------------------

   function Parse_Goto_Pose (Job : in Xml.Node) return Sancta.Plan.Subplan is
   begin
      return Sancta.Plan_Node.Create
        (Tasks.Goto_Pose.Create
           (Convert.To_Pose (Xml.Get_Attribute (Job, "goal", "")),
            Boolean'Value (Xml.Get_Attribute (Job, "use_angle", "true"))));
   end Parse_Goto_Pose;

   -------------------------
   -- Parse_Speed_Driving --
   -------------------------

   function Parse_Speed_Driving (Job : in Xml.Node) return Sancta.Plan.Subplan is
   begin
      return Sancta.Plan_Node.Create
        (Tasks.Speed_Driving.Create
           (Convert.To_Pose (Xml.Get_Attribute (Job, "velocity", "")),
            Duration'Value (Xml.Get_Attribute (Job, "period", ""))));
   end Parse_Speed_Driving;

   Parsers : constant array (Known_Kinds) of Parser :=
               (Choose_Entry_Point     => Parse_Entry_Point'Access,
                Explore_Segment        => Parse_Explore_Segment'Access,
                Explore_Directed_Segment  => Parse_Explore_Directed'Access,
                Goto_Pose              => Parse_Goto_Pose'Access,
                Speed_Driving          => Parse_Speed_Driving'Access);

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task (This : in out Object;
                       Job  : in     Sancta.Tasks.Object'Class)
   is
   begin
      if not Contains (This, Get_Id (Job)) then
         This.Plan.Add_Task (Job);
      end if;
   end Add_Task;

   ---------------
   -- Add_Tasks --
   ---------------

   procedure Add_Tasks (This : in out Object;
                        Jobs : in     Sancta.Tasks.Containers.Lists.List)
   is
      use Sancta.Tasks.Containers.Lists;
      I : Cursor := Jobs.First;
   begin
      while Has_Element (I) loop
         Add_Task (This, Element (I));
         Next (I);
      end loop;
   end Add_Tasks;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Object) is
   begin
      This.Plan.Clear;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains (This : in Object;
                      Job  : in Sancta.Tasks.Task_Id) return Boolean
   is
   begin
      return This.Plan.Contains (Job);
   end Contains;

   --------------
   -- Get_Plan --
   --------------

   function Get_Plan (This : in Object)
                      return    Sancta.Plan.Object
   is
   begin
      return This.Plan;
   end Get_Plan;

   -------------------
   -- Mark_Finished --
   -------------------

   procedure Mark_Finished (This : in out Object;
                            Id   : in     Sancta.Tasks.Task_Id)
   is
   begin
      if This.Contains (Id) then
         Sancta.Plan_Node.Set_Finished (This.Plan.Get_Node (Id));
      else
         Log ("Mission_Tracker.Mark_Finished: Task" & Id'Img &
              " unknown.", Warning, Log_Section);
      end if;
   end Mark_Finished;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File (This : in out Object;
                         Name : in     String)
   is
   begin
      if not Gnat.Os_Lib.Is_Readable_File (Name) then
         Log ("Mission_Tracker.Parse_File: Cannot open " & Name,
              Warning, Log_Section);
         return;
      end if;

      declare
         Mission :                Xml.Node := Xml.Parse (Name);
         Tasks   : constant Xml.Node_Array := Xml.Get_All (Mission, "task");
      begin
         Put_Line ("Mission: " & Name & " containing" &
                   Tasks'Length'Img & " tasks");
         for I in Tasks'Range loop
            declare
               Node  : constant Sancta.Plan.Subplan :=
                         Parsers
                           (Known_Kinds'Value
                              (Xml.Get_Attribute
                                 (Tasks (I), "kind", ""))).all (Tasks (I));
               Owner : constant String :=
                         Xml.Get_Attribute (Tasks (I), "owner", "any");
               Auct  : constant Boolean :=
                         Boolean'Value
                           (Xml.Get_Attribute (Tasks (I), "auctionable", "false"));
            begin
               if Owner /= "any" then
                  Sancta.Plan_Node.Set_Owner (Node, Owner);
               end if;
               if Auct then
                  Sancta.Plan_Node.Get_Task
                    (Node).all.Set_Property (Sancta.Tasks.Property_Auctionable,
                                             Auct'Img);
               end if;
               This.Plan.Add_Subplan (Node);
            end;
         end loop;
         Xml.Delete (Mission);
      end;
   end Parse_File;

   -------------------
   -- Print_Summary --
   -------------------

   procedure Print_Summary (This : in Object) is
   begin
      Put_Line ("*** MISSION SUMMARY ***");
      This.Plan.Print_Tree_Summary;
      Put_Line ("***     END SUMMARY ***");
   end Print_Summary;

end Sancta.Mission_Tracker;
