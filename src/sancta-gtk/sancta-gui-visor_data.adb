with Sancta.Auctions,
     Sancta.Bidder.Flat,
     Sancta.Tasks.Handle,
     Sancta.Types.Operations;

with Glib; use Glib;

with Ada.Unchecked_Deallocation;

package body Sancta.Gui.Visor_Data is

   ------------------
   -- Assign_Color --
   ------------------

   Colors : constant Ustring_Array := (+"Red", +"#00a000", +"Blue", +"Magenta");

   procedure Assign_Color (This : in out Robot_Data) is
      Name : constant String := This.Agent.Get_Name;
      Pos  : constant Integer :=
               Character'Pos (Name (Name'First)) - Character'Pos ('a') + 1;
   begin
      if Pos in Colors'Range then
         This.Gui_Color := Colors (Pos);
      else
         This.Gui_Color := +"Black";
      end if;
   end Assign_Color;

   -----------------
   -- Get_Mission --
   -----------------

   function Get_Mission (This : access Object)
                         return        Mission_Tracker.Object_Access
   is
   begin
      return This.Mission'Unchecked_Access;
      --  Unless unchecked, exception at runtime
      --  Yeah, this is unsafe shit
   end Get_Mission;

   ---------------
   -- Get_Robot --
   ---------------

   function Get_Robot (This : access Object;
                       Name : in     String) return Robot_Access
   is
      use Robot_Maps;
      I  : constant Cursor := Find (This.Robots, Name);
   begin
      if Has_Element (I) then
         return Element (I);
      else
         return null;
      end if;
   end Get_Robot;

   ---------------------
   -- Get_Robot_Color --
   ---------------------

   function Get_Robot_Color (Name : in String) return String is
      Pos  : constant Integer :=
               Character'Pos (Name (Name'First)) - Character'Pos ('A') + 1;
   begin
      if Pos in Colors'Range then
         return +Colors (Pos);
      else
         return "Black";
      end if;
   end Get_Robot_Color;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
      Types : constant GType_Array := (First_Column .. Last_Column => Gtype_String);
   begin
      Gtk_New (This.Logs, Types);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Auctioneer.Object'Class,
         Auctioneer.Object_Access);
   begin
      Free (This.Auctioneer);
   end Finalize;

   --------------
   -- Get_Logs --
   --------------

   function Get_Logs (This : in Object) return Gtk_Tree_Store is
   begin
      return This.Logs;
   end Get_Logs;

   ----------------
   -- Get_Robots --
   ----------------

   function Get_Robots (This : in Object) return Robot_Array is
      Result : Robot_Array (1 .. Natural (This.Robots.Length));
      Pos    : Positive := Result'First;

      use Robot_Maps;
      procedure Assign (I : in Cursor) is
      begin
         Result (Pos) := Element (I);
         Pos := Pos + 1;
      end Assign;

   begin
      This.Robots.Iterate (Assign'Access);

      return Result;
   end Get_Robots;

   ------------------
   -- Locate_Robot --
   ------------------

   function Locate_Robot (This : access Object;
                          Name : in     String) return Robot_Access
   is
      use Robot_Maps;
      I  : Cursor := Find (This.Robots, Name);
      Ok : Boolean;
   begin
      if Has_Element (I) then
         return Element (I);
      else
         Insert (This.Robots, Name, new Robot_Data, I, Ok); pragma Assert (Ok);
         Element (I).Agent.Set_Name (Name);
         Element (I).Agent.Set_Id (Value (Name));
         Assign_Color (Element (I).all);
         return Element (I);
      end if;
   end Locate_Robot;

   ------------------
   -- Locate_Robot --
   ------------------

   function Locate_Robot (This : Object_Access;
                          Pose : in     Types.Pose;
                          Dist : in     Float := 5.0) return Robot_Access
   is
      use Robot_Maps;
      use type Types.Real;
      I         : Cursor     := This.Robots.First;
      Best      : Cursor     := No_Element;
      Best_Dist : Types.Real := Types.Real'Last;
   begin
      while Has_Element (I) loop
         declare
            Curr_Dist : constant Types.Real :=
                          Types.Operations.Distance
                            (Pose,
                             Agent_Proxy.Get_Pose (Element (I).Agent));
         begin
            if Curr_Dist <= Types.Real (Dist) then
               if Best = No_Element or else Curr_Dist < Best_Dist then
                  Best_Dist := Curr_Dist;
                  Best      := I;
               end if;
            end if;
         end;

         Next (I);
      end loop;

      if Has_Element (Best) then
         return Element (Best);
      else
         return null;
      end if;
   end Locate_Robot;

   --------------------------
   -- Add_Task_For_Auction --
   --------------------------

   procedure Add_Task_For_Auction (This : in out Object;
                                   Job  : in     Sancta.Tasks.Object'Class)
   is
      Bis : Sancta.Tasks.Object'Class := Job;
   begin
      Bis.Set_Property (Tasks.Property_Auctionable, True'Img);
      This.Auctioneer.Add_Item
        (Bidder.Flat.Item'
           (Auctions.Items with Job => Tasks.Handle.Set (Bis)));
   end Add_Task_For_Auction;

   ------------------
   -- Run_Auctions --
   ------------------

   procedure Run_Auctions (This : in out Object) is
   begin
      Auctioneer.Run (This.Auctioneer.all);
   end Run_Auctions;

   --------------
   -- Get_Link --
   --------------

   function Get_Link (This : Object_Access) return Network.Layer.Object_Access
   is
   begin
      return This.Link;
   end Get_Link;

   --------------
   -- Set_Link --
   --------------

   procedure Set_Link (This : in out Object;
                       Link : in     Network.Layer.Object_Access)
   is
      use type Network.Layer.Object_Access;
   begin
      pragma Assert (This.Link = null);
      This.Link := Link;
      This.Auctioneer := new Sancta.Auctioneer.Object (This.Link);
      This.Auctioneer.Create (Auction_Channel);
   end Set_Link;

   ------------
   -- Config --
   ------------

   function Config (This : Object) return Sancta.Gui.Config.Object is
   begin
      return This.Options;
   end Config;

   ----------------
   -- Set_Config --
   ----------------

   procedure Set_Config (This : in out Object;
                         Opts :        Sancta.Gui.Config.Object)
   is
   begin
      This.Options := Opts;
   end Set_Config;

   ------------
   -- Drawer --
   ------------

   function Drawer (This : Object_Access)
   return Agpl.Drawing.Multisource.Object_Access
   is
   begin
      return This.Drawer'Access;
   end Drawer;

   ------------------
   -- Is_Recording --
   ------------------

   function Is_Recording (This : Object) return Boolean is
   begin
      return This.Recording;
   end Is_Recording;

   -------------------
   -- Set_Recording --
   -------------------

   procedure Set_Recording (This : in out Object;
                            Rec  :        Boolean := True)
   is
   begin
      This.Recording := Rec;
   end Set_Recording;

end Sancta.Gui.Visor_Data;
