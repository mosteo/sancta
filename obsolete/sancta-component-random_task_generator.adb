with Sancta.Component.Annealer;
with Sancta.Component.Factory;
pragma Elaborate_All (Sancta.Component.Factory);
with Sancta.Tasks.Explore_Segment;
with Sancta.Tasks.Goto_Pose;

with Agpl.Random;
with Agpl.Trace;

with Ada.Numerics.Elementary_Functions;

package body Sancta.Component.Random_Task_Generator is

   use Sancta.Types;
   use Agpl;
   use Agpl.Trace;
   use Ada.Calendar;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      use Agpl.Xml;
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      This.Xmin := Float'Value (Get_Attribute (Config, "xmin", This.Xmin'Img));
      This.Xmax := Float'Value (Get_Attribute (Config, "xmax", This.Xmax'Img));
      This.Ymin := Float'Value (Get_Attribute (Config, "ymin", This.Ymin'Img));
      This.Ymax := Float'Value (Get_Attribute (Config, "ymax", This.Ymax'Img));
      This.Length :=
        Float'Value (Get_Attribute (Config, "length", This.Length'Img));

      This.Delaymin :=
        Duration'Value (Get_Attribute (Config, "delaymin", This.Delaymin'Img));
      This.Delaymax :=
        Duration'Value (Get_Attribute (Config, "delaymax", This.Delaymax'Img));

      This.Num_Tasks :=
        Natural'Value (Get_Attribute (Config, "num_tasks", This.Num_Tasks'Img));

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      Added : Boolean;
   begin
      Next := Clock + 0.1;

      if This.Num_Tasks > 0 and then This.Timer.Elapsed >= This.Next_Delay then

         if This.Num_Tasks = 0 then
            Log ("Task generation finished", Informative);
         end if;

         This.Timer.Reset;
         This.Next_Delay :=
           Duration
             (Random.Get_Float (Float (This.Delaymin), Float (This.Delaymax)));

         if Random.Flip_Coin then
            Annealer.Add_Task
              (Tasks.Goto_Pose.Create
                 ((+Random.Get_Float (This.Xmin, This.Xmax),
                  +Random.Get_Float (This.Ymin, This.Ymax),
                  0.0),
                  Use_Angle => False), Added);
            if Added then
               This.Num_Tasks := This.Num_Tasks - 1;
               Log ("Random goal task created", Debug, Log_Section);
            end if;
         else
            loop
               declare
                  use Ada.Numerics.Elementary_Functions;
                  X1  : constant Float := Random.Get_Float (This.Xmin, This.Xmax);
                  Y1  : constant Float := Random.Get_Float (This.Ymin, This.Ymax);
                  A   : constant Float := Random.Get_Float (0.0, 3.1415926535);
                  X2  : constant Float := X1 + This.Length * Cos (A);
                  Y2  : constant Float := Y1 + This.Length * Sin (A);
               begin
                  if X2 <= This.Xmax and then X2 >= This.Xmin and then
                     Y2 <= This.Ymax and then Y2 >= This.Ymin
                  then
                     Annealer.Add_Task
                       (Tasks.Explore_Segment.Create
                          (From => (+X1, +Y1, 0.0),
                           To   => (+X2, +Y2, 0.0)), Added);
                     if Added then
                        This.Num_Tasks := This.Num_Tasks - 1;
                        Log ("Random segment task created", Debug, Log_Section);
                     end if;
                     exit;
                  end if;
               end;
            end loop;
         end if;
      end if;
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Random_Task_Generator;
