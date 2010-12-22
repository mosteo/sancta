with Agpl.Optimization.Concorde;
use  Agpl.Optimization.Concorde;
use  Agpl;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_Io; use Ada.Text_Io;

procedure Sancta.Main.Atsp is

   Cost : Cost_Matrices.Matrix (1 .. 12, 1 .. 12);
   type Cost_Array is array (Integer range <>) of Costs;

   Raw : constant Cost_Array (1 .. 144) :=
           (0,   75,  167,  151,   63,  198,  111,   78,  160, 106,  130,  128,
            82,    0,  201,  177,   76,  241,  109,   23,  199,  169,   59,   57,
            171,  206,    0,   41,  136,   49,  115,  192,   19,  105,  244,  243,
            154,  181,   30,    0,  112,   78,   86,  166,   37,  106,  217,  215,
            68,   83,  125,  104,    0,  165,   53,   70,  123,  106,  125,  124,
            192,  237,   52,  81,  167,    0,  152, 224,   46,  108,  278,  277,
            116,  109,  113,  86,   55,  161,    0,   91,  117,  134,  135,  134,
            78,   26,  184,  160,   61,  226,   92,    0,  183,  157,   69,   67,
            157,  194,   19,   39,  124,   56,  107,  180,    0,   91,  233,  232,
            107,  169,   99, 104,  110,   108, 128,  163,   87,    0,  220,  219,
            134,   61,  235,  209,  118,  279,  134,   62,  236,  217,    0,    8,
            132,   59,  234,  208,  116,  278,  133,   60,  235,  216,    8,    0);

   Idx : Integer := Raw'First;
begin
   for Row in Cost'Range loop
      for Col in Cost'Range (2) loop
         Cost (Row, Col) := Raw (Idx);
         Idx := Idx + 1;
      end loop;
   end loop;
   pragma Assert (Idx = Raw'Last + 1);

   --  Nullify return.
   for Row in Cost'Range loop
      for Col in Cities'(11) .. 12 loop
         Cost (Row, Col) := 0;
      end loop;
   end loop;

   Print_Problem (Cost);

   for I in 1 .. 100 loop
      declare
         Sol : constant Result_Matrix := Solve_Atsp ((1 => 11), Cost);
      begin
         Print_Solution (Cost, (1 => 11), Sol, No_Return => True);
         if Get_Total_Cost (Cost, Sol, No_Return => True) /= 575 then
            Put_Line ("OH THE HORROR!!" & I'Img);
            delay 24.0 * 60.0 * 60.0;
            raise Constraint_Error;
         end if;
      end;
      Put_Line ("WYE WYE" & I'Img);
   end loop;
exception
   when E : others =>
      Put_Line (Exception_Information (E) & " " & Exception_Message (E));
end Sancta.Main.Atsp;
