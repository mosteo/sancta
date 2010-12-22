package body Sancta.Cost_Cache.Homogeneous is

   package Ac renames Agent.Containers;
   package Tc renames Sancta.Tasks.Containers;

   use type Sancta.Tasks.Task_Id;

   ---------
   -- Key --
   ---------
   function Key (Agent : in String;
                 Ini   : in Sancta.Tasks.Task_Id;
                 Fin   : in Sancta.Tasks.Task_Id) return String;
   pragma Inline (Key);
   function Key (Agent : in String;
                 Ini   : in Sancta.Tasks.Task_Id;
                 Fin   : in Sancta.Tasks.Task_Id) return String
   is
   begin
      if Ini = Sancta.Tasks.No_Task then
         return Agent & ":" & Ini'Img & ":" & Fin'Img;
      else
         return "Any:" & Ini'Img & ":" & Fin'Img;
      end if;
   end Key;

   --------------
   -- Set_Cost --
   --------------

   procedure Set_Cost
     (This  : in out Object;
      Agent : in     String;
      Ini   : in     Sancta.Tasks.Task_Id;
      Fin   : in     Sancta.Tasks.Task_Id;
      Cost  : in     Costs)
   is
   begin
      This.Cache.Include (Key (Agent, Ini, Fin), Cost);
   end Set_Cost;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id)
      return Costs
   is
      I : constant Cursor := Find (This.Cache, Key (Agent, Ini, Fin));
   begin
      if I = No_Element then
         if Fin = Sancta.Tasks.No_Task then
            return 0.0;
         else
            return Infinite;
         end if;
      else
         return Element (I);
      end if;
   end Get_Cost;

   --------------
   -- Contains --
   --------------

   function Contains
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Boolean is
   begin
      return This.Cache.Contains (Key (Agent, Ini, Fin));
   end Contains;

   ------------------
   -- Add_Initials --
   ------------------

   procedure Add_Initials
     (This   : in out Object;
      Agents :        Agent.Containers.Lists.List;
      Tasks  :        Sancta.Tasks.Containers.Lists.List)
   is
      procedure For_Agent (I : Ac.Lists.Cursor) is
         Agent : constant Sancta.Agent.Object'Class := Ac.Lists.Element (I);

         procedure For_Task (J : Tc.Lists.Cursor) is
            Job : constant Sancta.Tasks.Object'Class := Tc.Lists.Element (J);
         begin
            This.Cache.Insert
              (Key (Agent.Get_Name, Sancta.Tasks.No_Task, Job.Get_Id),
               Agent.Get_Cost (Job));
         end For_Task;

      begin
         Tasks.Iterate (For_Task'Access);
      end For_Agent;
   begin
      Agents.Iterate (For_Agent'Access);
   end Add_Initials;

   ------------
   -- Create --
   ------------

   function Create
     (Agents : Agent.Containers.Lists.List;
      Tasks  : Sancta.Tasks.Containers.Lists.List)
      return Object
   is
      This : Object;

      ----------------------
      -- Create_For_Tasks --
      ----------------------

      procedure Create_For_Tasks is
         Agent : constant Sancta.Agent.Object'Class := Agents.First_Element;

         procedure Loop1 (I : Tc.Lists.Cursor) is
            Ini : Sancta.Tasks.Object'Class renames Tc.Lists.Element (I);

            procedure Loop2 (J : Tc.Lists.Cursor) is
               Fin : Sancta.Tasks.Object'Class renames Tc.Lists.Element (J);
            begin
               This.Cache.Insert
                 (Key (Agent.Get_Name, Ini.Get_Id, Fin.Get_Id),
                  Agent.Get_Cost (Ini, Fin));
            end Loop2;

         begin
            Tasks.Iterate (Loop2'Access);
            This.Cache.Include
                 (Key (Agent.Get_Name, Ini.Get_Id, Sancta.Tasks.No_Task),
                  0.0);
         end Loop1;

      begin
         Tasks.Iterate (Loop1'Access);
      end Create_For_Tasks;

   begin
      if Agents.Is_Empty then
         return This;
      end if;

      Add_Initials (This, Agents, Tasks);
      Create_For_Tasks;

      return This;
   end Create;

end Sancta.Cost_Cache.Homogeneous;
