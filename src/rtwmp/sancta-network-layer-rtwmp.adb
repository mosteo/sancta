with Agpl.Strings;
with Agpl.Strings.Fields;
with Interfaces.C;
with Sancta.Component.Helper;
with Sancta.Network.Layer.Utils;

package body Sancta.Network.Layer.Rtwmp is

   package Ic renames Standard.Interfaces.C;
   use type Ic.Char;
   use type Ic.Int;

   ---------
   -- Mtu --
   ---------

   function Max_Data_Size return Stream_Element_Offset is
      function Mtu_C return Ic.Size_T;
      pragma Import (C, Mtu_C, "sancta_rtwmp_max_data_size");
   begin
      return Stream_Element_Offset (Mtu_C);
   end Max_Data_Size;

   -----------------------
   -- To_Signal_Quality --
   -----------------------

   function To_Signal_Quality (Q : Ic.Char) return Signal_Quality is
      Max : constant Signal_Quality'Base := 100.0;
   begin
      return Signal_Quality'Base (Ic.Char'Pos (Q)) * Signal_Quality'Last / Max;
   end To_Signal_Quality;

   -------------
   -- To_Char --
   -------------

   function To_Char (Addr : Rtwmp_Address) return Ic.Char is
   begin
      return Ic.Char'Val (Addr);
   end To_Char;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object;
                         Conf :        Agpl.Xml.Node)
   is
      use Agpl.Strings;
      use Agpl.Strings.Fields;
      use Agpl.Xml;

      procedure Wmprunbg;
      pragma Import (C, Wmprunbg, "wmpRunBG");
      function Wmpsetup (Id : Ic.Char; Num_Nodes : Ic.Char) return Ic.Int;
      pragma Import (C, Wmpsetup, "wmpSetup");

      Nodes : constant String :=
                Trim (L (Component.Helper.Option (Conf, "nodes")));
   begin
      for I in 1 .. Num_Tokens (Nodes, '.') loop
         declare
            Id   : constant String := Select_Field (Nodes, I, '.');
            Addr : constant String := Trim (Natural'Image (I - 1));
         begin
            Log ("Node Id/Addr ENABLED: " & Id & "/" & Addr,
                 Informative, Log_Section);
            This.Add_Node (Value (Id), Addr);
         end;
      end loop;

      Utils.Add_Groups (This, Conf);

      if Wmpsetup (Ic.Char'Val (Value (This.Get_Address (This.Id))),
                   Ic.Char'Val (This.Num_Nodes)) /= 0 then null; end if;
      Wmprunbg;

      This.Start_Listening;
   end Initialize;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (This : in out Object) is
      function Wmpexit return Ic.Int;
      pragma Import (C, Wmpexit, "wmpExit");
   begin
      Root.Object (This).Shutdown;
      if Wmpexit /= 0 then
         null;
      end if;
   end Shutdown;

   subtype Buffer_Type is Stream_Element_Array (0 .. Max_Data_Size - 1);

   -------------
   -- Receive --
   -------------

   function Receive (This : Object) return Stream_Element_Array is
      pragma Unreferenced (This);
      procedure Receive_C (Buffer : in out Buffer_Type;
                           Last   : out Ic.Size_T);
      pragma Import (C, Receive_C, "sancta_rtwmp_receive");

      Buffer : aliased Buffer_Type;
      Last   : Ic.Size_T;
   begin
      Log ("Blocking at receive...", Debug, Log_Section);
      Receive_C (Buffer, Last);
      Log ("Received bytes:" & Last'Img, Debug, Log_Section);
      return Buffer (0 .. Stream_Element_Offset (Last));
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send (This : in out Object;
                   Dest : in     Node_Id;
                   Data : in     Stream_Element_Array)
   is
      Buffer : aliased Buffer_Type;
      procedure Send_C (Buffer : in out Buffer_Type;
                        Last   :        Ic.Size_T;
                        Src    :        Ic.Char;
                        Dst    :        Ic.Char;
                        Ok     :    out Ic.Int); -- 1 success, 0 queue full
      pragma Import (C, Send_C, "sancta_rtwmp_send");
      Ok : Ic.Int := 2;
   begin
      if Data'Length > Max_Data_Size then
         raise Constraint_Error
           with "RT-WMP: packet too large, bytes:" & Data'Length'Img;
      end if;

      Log ("Sending to " & Image (Dest) & " bytes:" & Data'Length'Img, Debug, Log_Section);
      Buffer (0 .. Data'Length - 1) := Data;
      Send_C (Buffer => Buffer,
              Last   => Ic.Size_T (Data'Length - 1),
              Src    => To_Char (Rtwmp.Value (This.Get_Address (This.Id))),
              Dst    => To_Char (Rtwmp.Value (This.Get_Address (Dest))),
              Ok     => Ok);

      case Ok is
         when 0 =>
            raise Constraint_Error with "RT-WMP queue full";
         when 1 =>
            null;
         when 2 =>
            raise Program_Error with "Return code untouched";
         when others =>
            raise Program_Error with "Unknown return code in RTWMP:" & Ok'Img;
      end case;
   end Send;

   -----------------
   -- Get_Quality --
   -----------------

   function Get_Quality (This : Object) return Quality_Matrix is
      type CQ_Matrix is array
        (0 .. Rtwmp_Address (This.Num_Nodes - 1),
         0 .. Rtwmp_Address (This.Num_Nodes - 1)) of aliased Ic.Char;
      pragma Convention (C, CQ_Matrix);
      type Cq_Access is access all Cq_Matrix;
      pragma Convention (C, Cq_Access);

      function Get_Lqm (Q : Cq_Access) return Ic.Int;
      pragma Import (C, Get_Lqm, "wmpGetLatestLQM");

      Q  : Quality_Matrix (0 .. Rtwmp_Address (This.Num_Nodes - 1),
                           0 .. Rtwmp_Address (This.Num_Nodes - 1));
      Cq : aliased Cq_Matrix;
   begin
      if Get_Lqm (Cq'Access) /= 0 then
         null;
      end if;

      for R in Cq'Range loop
         for C in Cq'Range (2) loop
            Q (R, C) := To_Signal_Quality (Cq (R, C));
         end loop;
      end loop;

      return Q;
   end Get_Quality;

   -----------
   -- Image --
   -----------

   function Image (Addr : Rtwmp_Address) return Protocol_Specific_Address is
   begin
      return Agpl.Strings.Trim (Addr'Img);
   end Image;

end Sancta.Network.Layer.Rtwmp;
