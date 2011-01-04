with Sancta.Datastore;

with Agpl.Protected_Datastore; use Agpl.Protected_Datastore;

package body Sancta.Ctree.Data_Server is

   package Sd renames Sancta.Datastore;

   Dsid : constant Sancta.Node_Id := Sancta.Value ("dataserver");

   type Safe_Pose is new Sd.Object_Data with record
      Pose : Poses;
   end record;

   type Safe_Goal is new Sd.Object_Data with record
      Goal : Goals;
   end record;

   type Safe_Link is new Sd.Object_Data with record
      Link : Links;
   end record;

   subtype Safe is Sd.Object_Data;

   --------------
   -- Set_Pose --
   --------------

   procedure Set_Pose (Id : Robot_Id; Pose :     Poses) is
   begin
      Sd.Object (Dsid).Set(Id'Img & ":nerus:pose", Safe_Pose'(Safe with Pose));
   end Set_Pose;

   --------------
   -- Get_Pose --
   --------------

   procedure Get_Pose (Id : Robot_Id; Pose : out Poses) is
   begin
      declare
         S : constant Safe'Class := Sd.Object (Dsid).Get (Id'Img & ":nerus:pose");
      begin
         Pose := Safe_Pose (S).Pose;
      end;
   exception
      when Data_Not_Present =>
         Pose := (0.0, 0.0, 0.0);
   end Get_Pose;

   --------------
   -- Set_Goal --
   --------------

   procedure Set_Goal (Id : Robot_Id; Goal : Goals) is
   begin
      Sd.Object (Dsid).Set(Id'Img & ":nerus:goal", Safe_Goal'(Safe with Goal));
   end Set_Goal;

   --------------
   -- Get_Goal --
   --------------

   procedure Get_Goal (Id : Robot_Id; Goal : out Goals) is
   begin
      declare
         S : constant Safe'Class := Sd.Object (Dsid).Get (Id'Img & ":nerus:goal");
      begin
         Goal := Safe_Goal (S).Goal;
      end;
   exception
      when Data_Not_Present =>
         Goal := (0.0, 0.0, Active => False);
   end Get_Goal;

   ---------------
   -- Set_Links --
   ---------------

   procedure Set_Link (R1, R2 : Robot_Id; Link :     Links) is
   begin
      Sd.Object (Dsid).Set(R1'Img & ":" & R2'Img & ":nerus:link",
                    Safe_Link'(Safe with Link));
   end Set_Link;

   ---------------
   -- Get_Links --
   ---------------

   procedure Get_Link (R1, R2 : Robot_Id; Link : out Links) is
   begin
      declare
         S : constant Safe'Class := Sd.Object (Dsid).Get (R1'Img & ":" & R2'Img &
                                                   ":nerus:link");
      begin
         Link := Safe_Link (S).Link;
      end;
   exception
      when Data_Not_Present =>
         if R1 > R2 then
            Get_Link (R1 => R2, R2 => R1, Link => Link);
         else
            Link := Links'Last;
         end if;
   end Get_Link;

   ----------
   -- Goal --
   ----------

   function Goal (X, Y : Float; Active : Boolean := True) return Goals is
   begin
      return (C.Double(X), C.Double(Y), Active);
   end Goal;

   function Goal (P : Sancta.Types.Pose) return Goals is
   begin
      return (C.Double (P.X), C.Double (P.Y), True);
   end Goal;

   function "+" (L : Links) return Link_Qualities is
   begin
      return Link_Qualities (L);
   end "+";

   function "+" (P : Poses) return Sancta.Types.Pose is
      use Sancta.Types;
   begin
      return (Real (P.X), Real (P.Y), Angle (P.A));
   end "+";

   ----------
   -- Pose --
   ----------

   function Pose (P : Sancta.Types.Pose) return Poses is
   begin
      return (C.Double (P.X), C.Double (P.Y), C.Double (P.A));
   end Pose;

   function "+" (G : Goals) return Goal_Ada is
   begin
      return (Float (G.X), Float (G.Y), G.Active);
   end "+";

end Sancta.Ctree.Data_Server;
