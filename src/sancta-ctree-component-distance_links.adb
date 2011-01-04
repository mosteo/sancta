with Sancta.Ctree.CTTypes,
     Sancta.Ctree.Connectivity_Matrix,
     Sancta.Located_Agent,
     Sancta.Assignment,
     Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Containers,
     Sancta.Types,
     Sancta.Types.Operations;

use Sancta.Containers;

package body Sancta.Ctree.Component.Distance_Links is

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
     (Config : in Agpl.Xml.Node)
      return Sancta.Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Subscribe (Requires_Team);
      return Sancta.Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
      use Ac.Lists, Sancta.Types, Sancta.Types.Operations;

      Team  : Assignment.Object renames Sancta.Component.Types.Teams (Value).Team;
      Bots  : constant Ac.Lists.List := Team.Get_Agents;
      I     : Cursor := Bots.First;
      Links : Connectivity_Matrix.Object;
   begin
      This.Verify (Option_Threshold);
      Links.Set_Umbral
        (Link_Qualities'Value (This.Option (Option_Threshold, "0.0")));

      while Has_Element (I) loop
         declare
            J : Cursor := Bots.First;
         begin
            while Has_Element (J) loop
               if I /= J then
                  Links.Set_Link
                    (Element (I).Get_Name,
                     Element (J).Get_Name,
                     To_Quality
                       (Float
                          (Distance
                             (Located_Agent.Object'Class
                                (Element (I)).Get_Pose,
                              Located_Agent.Object'Class
                                (Element (J)).Get_Pose))));
               end if;
               Next (J);
            end loop;
         end;
         Next (I);
      end loop;

--        Log ("LINKS DISTANCED", Always);
--        Links.Print;

      This.Output (Provides_Links, CTTypes.Links'(Data with Links));
   end Key_Stored;

end Sancta.Ctree.Component.Distance_Links;
