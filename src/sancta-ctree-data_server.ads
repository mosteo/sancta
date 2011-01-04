with Sancta.Types;

private with Interfaces.C;

package Sancta.Ctree.Data_Server is

--   pragma Preelaborate;

   type Poses is private;
   type Goals is private;
   type Links is private; -- Network connectivities

   type Goal_Ada is record
      X, Y   : Float;
      Active : Boolean;
   end record;

   type Goal_Array is array (Positive range <>) of Goal_Ada;

   --  These are callable from C code!

   procedure Set_Pose (Id : Robot_Id; Pose :     Poses);
   procedure Get_Pose (Id : Robot_Id; Pose : out Poses);

   procedure Set_Goal (Id : Robot_Id; Goal :     Goals);
   procedure Get_Goal (Id : Robot_Id; Goal : out Goals);

   procedure Set_Link (R1, R2 : Robot_Id; Link :     Links);
   procedure Get_Link (R1, R2 : Robot_Id; Link : out Links);

   --  Ada side utilities
   function "+" (L : Links) return Link_Qualities; pragma Inline ("+");
   function Goal (X, Y : Float; Active : Boolean := True) return Goals; pragma Inline (Goal);
   function Goal (P : Sancta.Types.Pose) return Goals; pragma Inline (Goal);
   function "+" (P : Poses) return Sancta.Types.Pose; pragma Inline ("+");
   function Pose (P : Sancta.Types.Pose) return Poses; pragma Inline (Pose);
   function "+" (G : Goals) return Goal_Ada; pragma Inline ("+");

private

   use Interfaces;

   type Poses is record
      X, Y, A : C.double;
   end record;
   pragma Convention (C, Poses);

   type Goals is record
      X, Y    : C.double;
      Active  : Boolean;
   end record;
   pragma Convention (C, Goals);

   type Links is new C.Double range 0.0 .. C.Double (Float'Last);

   pragma Export (C, Set_Pose);
   pragma Export (C, Get_Pose);
   pragma Export (C, Set_Goal);
   pragma Export (C, Get_Goal);
   pragma Export (C, Set_Link);
   pragma Export (C, Get_Link);

end Sancta.Ctree.Data_Server;
