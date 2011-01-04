with Agpl.Drawing;
with Sancta.Ctree.Connectivity_Matrix;
with Sancta.Ctree;
with Sancta.Ctree.Team_Tree;
with Sancta.Ctree.Tree_Navigator;
with Sancta.Ctree.Robot.Player;
with Sancta.Component;
with Sancta.Network.Qualities;

use Sancta.Component;

package Sancta.Ctree.CTypes is

   --  Types for the ctree components

   subtype Data is Sancta.Component.Data;

   type Robots is new Data with record
      Bot : Robot.Player.Object;
   end record;

   type Clusters is new Data with record
      Links : Connectivity_Matrix.Object;
   end record;
   subtype Links is Clusters;

   type Tree_Nav is new Data and Agpl.Drawing.Drawable with record
      Tree : Ctree.Tree_Navigator.Handle.Object;
   end record;

   overriding
   procedure Draw (This :        Tree_Nav;
                   Dest : in out Agpl.Drawing.Drawer'Class);

   type Team_Tree is new Data with record
      Team : Ctree.Team_Tree.Object;
   end record;

   type Signal is new Data with record
      Links : Ctree.Id_Q_Maps.Map;
   end record;

   type Full_Signal
     (Directed_Source   : Boolean;
      Directed_Query    : Boolean;
      Missing_As_Broken : Boolean) is new Data with
      record
         Links : Sancta.Network.Qualities.Map
           (Directed_Source,
            Directed_Query,
            Missing_As_Broken);
      end record;


end Sancta.Ctree.CTypes;
