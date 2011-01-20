package body Sancta.Ctree.Team_Tree is

   ------------
   -- Create --
   ------------

   function Create
     (From : Assignment.Object)
      return Object'Class
   is
      This : Object;
   begin
      This.Set_Agents (From.Get_Agents);
      return This;
   end Create;

   ------------
   -- Parent --
   ------------

   function Parent
     (This : Object;
      Bot  : String)
      return Located_Agent.Object'Class
   is
   begin
      return
        Located_Agent.Object'Class(This.Get_Agent (This.Parents.Element (Bot)));
   end Parent;

   function Parent (This : Object;
                    Bot  : Node_Id) return Node_Id is
   begin
      return +This.Parents.Element (-Bot);
   end Parent;

   --------------
   -- Children --
   --------------

   function Children
     (This : Object;
      Bot  : String)
      return Ac.Lists.List
   is
      Result : Ac.Lists.List;
      use Agpl.Containers.String_String_Maps;
      procedure Children (I : Cursor) is
      begin
         if Element (I) = Bot then
            Result.Append (This.Get_Agent (Key (I)));
         end if;
      end Children;
   begin
      This.Parents.Iterate (Children'Access);
      return Result;
   end Children;

   --------------
   -- Children --
   --------------

   function Children (This : Object;
                      Bot  : Node_Id) return Id_Vectors.Vector
   is
      Result : Id_Vectors.Vector;
      use Agpl.Containers.String_String_Maps;
      procedure Children (I : Cursor) is
      begin
         if Element (I) = Bot then
            Result.Append (+Key (I));
         end if;
      end Children;
   begin
      This.Parents.Iterate (Children'Access);
      return Result;
   end Children;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (This   : in out Object;
      Child  :        String;
      Parent :        String)
   is
   begin
      This.Parents.Include (Child, Parent);
   end Set_Parent;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (This   : in out Object;
                         Child  :        Node_Id;
                         Parent :        Node_Id)
   is
   begin
      This.Parents.Include (-Child, -Parent);
   end Set_Parent;

   ------------
   -- Create --
   ------------

   function Create (Agent : Sancta.Agent.Object'Class) return Object is
   begin
      return Object (Create (Assignment.Create (Agent)));
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Agents : AC.Lists.List)
                    return   Object is
   begin
      return Object (Create (Assignment.Create (Agents)));
   end Create;

end Sancta.Ctree.Team_Tree;
