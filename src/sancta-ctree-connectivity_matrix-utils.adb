with Sancta.Ctree.Data_Server;
with Sancta.Ctree.Weak_Grouping;

with Sancta.Agent.Containers;

with Agpl.Containers.String_Sets;

package body Sancta.Ctree.Connectivity_Matrix.Utils is

   package Ac renames Sancta.Agent.Containers;

   Umbral : Link_Qualities;

   ----------------
   -- Set_Umbral --
   ----------------

   procedure Set_Umbral (U : Link_Qualities) is
   begin
      Umbral := U;
   end Set_Umbral;

   --------------------
   -- From_Datastore --
   --------------------

   function From_Datastore (Num_Bots : Natural) return Object
   is
      use type Data_Server.Links;
      This : Object;
   begin
      for I in 1 .. Num_Bots - 1 loop
         for J in I + 1 .. Num_Bots loop
            declare
               Link : Data_Server.Links;
               Id_1 : constant Robot_Id := Value (I'Img);
               Id_2 : constant Robot_Id := Value (J'Img);
            begin
               Data_Server.Get_Link (Id_1, Id_2, Link);

               --  Only insert the ones in the tree:
               if +Link > 0.0 then
                  This.Set_Link (Image (Id_1), Image (Id_2), +Link);
               end if;
            end;
         end loop;
      end loop;

      This.Set_Umbral (Umbral);

      return This;
   end From_Datastore;

   ---------------------
   -- Merge_Keep_Weak --
   ---------------------

   procedure Merge_Keep_Weak (This : in out Object; Add : Object) is
      use Link_Maps;
      I : Cursor := Add.First;
   begin
      while Has_Element (I) loop
         declare
            Bots : constant Key := Link_Maps.Key (I);
         begin
            if This.Contains (Bots) then
               if Add.Is_Weak (Bots) or else not This.Is_Weak (Bots) then
                  This.Include (Bots, Element (I));
               end if;
            else
               This.Insert (Bots, Element (I));
            end if;
         end;

         Next (I);
      end loop;
   end Merge_Keep_Weak;

   -------------------------------------
   -- Make_All_Weak_For_Idle_Clusters --
   -------------------------------------

   procedure Make_All_Weak_For_Idle_Clusters (This : in out Object;
                                              Ass  :        Assignment.Object)
   is
      use Agpl.Containers.String_Sets;

      Groups : constant Weak_Grouping.Object :=
                 Weak_Grouping.Create (Ass, This);
      Idle   : Set;
   begin
      for I in 1 .. Groups.Num_Groups loop
         declare
            procedure Query (I : Ac.Lists.Cursor) is
            begin
               Idle.Include (Ac.Lists.Element (I).Get_Name);
            end Query;
         begin
            if Groups.Get_Group (I).Get_Non_Idle_Agents.Is_Empty then
               Groups.Get_Group (I).Get_Idle_Agents.Iterate (Query'Access);
            end if;
         end;
      end loop;

      This.Make_All_Weak (Idle);
   end Make_All_Weak_For_Idle_Clusters;

   --------------------
   -- Keep_Only_Tree --
   --------------------

   function Keep_Only_Tree (This : Object;
                            Tree : Ctree.Team_Tree.Object'Class)
                            return Object
   is
      Result : Object;

      use Agpl.Containers.String_Sets;
      procedure Keep (I : Cursor) is
         Child  : constant String := Element (I);
         Parent : constant String := Tree.Parent (Element (I)).Get_Name;
      begin
         if This.Contains (Child, Parent) then
            Result.Set_Link
              (Child, Parent, This.Get_Link (Child, Parent));
         end if;
      end Keep;
   begin
      Result.Set_Umbral (This.Umbral);
      This.Get_Robots.Iterate (Keep'Access);
      return Result;
   end Keep_Only_Tree;

end Sancta.Ctree.Connectivity_Matrix.Utils;
