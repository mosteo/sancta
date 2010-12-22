with Agpl.Trace; use Agpl.Trace;
use Agpl;

package body Sancta.Cost_Proxy is

   use type Sancta.Costs;
   use type Sancta.Tasks.Task_Id;

   No_Task : Sancta.Tasks.Task_Id renames Sancta.Tasks.No_Task;

   --------------
   -- Set_Cost --
   --------------

   procedure Set_Cost
     (This  : in out Object;
      Agent : in     String;
      Ini   : in     Sancta.Tasks.Task_Id;
      Fin   : in     Sancta.Tasks.Task_Id;
      Cost  : in     Costs) is
   begin
      raise Program_Error with "Makes no sense for Cost_Proxy";
   end Set_Cost;

   overriding
   function Contains
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Boolean
   is
      pragma Unreferenced (Ini, Fin);
   begin
      return This.Agents.Contains (Agent);
   end Contains;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id)
      return Sancta.Costs
   is
      use Agent_Maps;
      use Task_Maps;
      A  : constant Agent_Maps.Cursor       := This.Agents.Find (Agent);
      I  : constant Task_Maps.Cursor        := This.Tasks.Find (Ini);
      F  : constant Task_Maps.Cursor        := This.Tasks.Find (Fin);
      H  : constant String_Cost_Maps.Cursor := This.Historics.Find (Agent);
      Hc :          Sancta.Costs;
   begin
      if Has_Element (A) then

         if String_Cost_Maps.Has_Element (H) then
            Hc := String_Cost_Maps.Element (H);
         else
            Hc := 0.0;
         end if;

         if Has_Element (I) and then Has_Element (F) then
            return Agent_Proxy.Object
              (Element (A)).Get_Cost (Element (I), Element (F));
         elsif Ini = No_Task and then Fin = No_Task then
            return 0.0;
         elsif Ini = No_Task and then not Has_Element (F) then
            return Sancta.Infinite;
         elsif Ini = No_Task then
            if This.Historic_Tasks.Contains (Agent) then
               return
                 Hc + Agent_Proxy.Object
                   (Element (A)).Get_Cost
                     (This.Historic_Tasks.Element (Agent),
                      Element (F));
            else
               return
                 Hc + Agent_Proxy.Object (Element (A)).Get_Cost (Element (F));
            end if;
         elsif Fin = No_Task then
            return 0.0;
         else
            return Sancta.Infinite; -- Missing task
         end if;
      else
         return Sancta.Infinite; -- Missing agent
      end if;
   exception
      when E : others =>
         Log ("Cost_Proxy:" & Ini'Img & Fin'Img & ": " & Report (E),
              Error, Log_Section);
         Log ("In historics: " & Agent & ": " &
              This.Historic_Tasks.Contains (Agent)'Img & " Id =" &
              This.Historic_Tasks.Element (Agent).Get_Id'Img,
              Error, Log_Section);
         raise;
   end Get_Cost;

   -------------------
   -- Exclude_Agent --
   -------------------

   procedure Exclude_Agent (This  : in out Object;
                            Agent : in     String)
   is
   begin
      This.Agents.Exclude (Agent);
   end Exclude_Agent;

   ---------------
   -- Set_Agent --
   ---------------

   procedure Set_Agent
     (This  : in out Object;
      Agent : in     Agent_Proxy.Object'Class)
   is
   begin
      This.Agents.Include (Agent.Get_Name, Agent);
   end Set_Agent;

   -----------------------
   -- Clear_All_History --
   -----------------------

   procedure Clear_All_History (This : in out Object) is
   begin
      This.Historics.Clear;
      This.Historic_Tasks.Clear;
   end Clear_All_History;

   -----------------------------
   -- Get_Agent_Historic_Cost --
   -----------------------------

   function Get_Agent_Historic_Cost (This  : in Object;
                                     Agent : in String) return Sancta.Costs
   is
   begin
      if This.Historics.Contains (Agent) then
         return This.Historics.Element (Agent);
      else
         return 0.0;
      end if;
   end Get_Agent_Historic_Cost;

   --------------------------------
   -- Add_To_Agent_Historic_Cost --
   --------------------------------

   procedure Add_To_Agent_Historic_Cost
     (This  : in out Object;
      Agent : in     String;
      Cost  : in     Sancta.Costs)
   is
      use String_Cost_Maps;
      I : constant Cursor := This.Historics.Find (Agent);
   begin
      if Has_Element (I) then
         This.Historics.Include (Agent, Element (I) + Cost);
      else
         This.Historics.Insert (Agent, Cost);
      end if;
   end Add_To_Agent_Historic_Cost;

   -----------------------------
   -- Set_Agent_Historic_Cost --
   -----------------------------

   procedure Set_Agent_Historic_Cost
     (This  : in out Object;
      Agent : in     String;
      Cost  : in     Sancta.Costs)
   is
   begin
      This.Historics.Include (Agent, Cost);
   end Set_Agent_Historic_Cost;

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task (This : in out Object;
                       Job  : in     Sancta.Tasks.Object'Class) is
   begin
      This.Tasks.Include (Job.Get_Id, Job);
   end Add_Task;

   ---------------
   -- Add_Tasks --
   ---------------

   procedure Add_Tasks (This  : in out Object;
                        Tasks : in     Task_Lists.List)
   is
      use Task_Lists;
      procedure Do_It (I : Task_Lists.Cursor) is
      begin
         This.Tasks.Include (Element (I).Get_Id, Element (I));
      end Do_It;
   begin
      Tasks.Iterate (Do_It'Access);
   end Add_Tasks;

   --------------
   -- Contains --
   --------------

   function Contains (This : in Object;
                      Job  : in Sancta.Tasks.Task_Id) return Boolean is
   begin
      return Job = No_Task or else This.Tasks.Contains (Job);
   end Contains;

   -------------------------------
   -- Clear_Agent_Historic_Task --
   -------------------------------

   procedure Clear_Agent_Historic_Task
     (This  : in out Object;
      Agent : in     String)
   is
   begin
      This.Historic_Tasks.Exclude (Agent);
   end Clear_Agent_Historic_Task;

   -----------------------------
   -- Set_Agent_Historic_Task --
   -----------------------------

   procedure Set_Agent_Historic_Task
     (This  : in out Object;
      Agent : in     String;
      Job   : in     Sancta.Tasks.Object'Class)
   is
   begin
      This.Historic_Tasks.Include (Agent, Job);
   end Set_Agent_Historic_Task;

end Sancta.Cost_Proxy;
