package body Sancta.Network.Inbox is

   ------------------
   -- On_Reception --
   ------------------

   procedure On_Reception (This : in out Object;
                           M    : in     Message'Class;
                           Meta : in     Message_Metadata) is
   begin
      This.Safe.Put (M, Meta);
   end On_Reception;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : in Object) return Boolean is
   begin
      return This.Safe.Is_Empty;
   end Is_Empty;

   ---------------
   -- Get_First --
   ---------------

   function Get_First (This : in Object) return Message'Class is
   begin
      return This.Safe.Get_First;
   end Get_First;

   ------------------------
   -- Get_First_Metadata --
   ------------------------

   function Get_First_Metadata (This : in Object) return Message_Metadata is
   begin
      return This.Safe.Get_First_Metadata;
   end Get_First_Metadata;

   ------------------
   -- Remove_First --
   ------------------

   procedure Remove_First (This : in out Object) is
   begin
      This.Safe.Remove_First;
   end Remove_First;

   ----------
   -- Open --
   ----------

   procedure Open (This  : in out Object;
                   Read  :        access procedure (Msg  : Message'Class;
                                    Meta : Message_Metadata);
                   Block :        Boolean := True)
   is
      Msg  : Handles.Message_Handle;
      Meta : Handles.Metadata_Handle;
   begin
      if Block then
         This.Safe.Remove_First_Blocking (Msg, Meta);
         Read (Msg.Ref.all, Meta.Ref.all);
      else
         This.Safe.Remove_First_Non_Blocking (Msg, Meta);
         if Msg.Is_Valid then
            Read (Msg.Ref.all, Meta.Ref.all);
         end if;
      end if;
   end Open;

   -----------------
   -- Safe_Object --
   -----------------

   use Message_Lists;
   use Metadata_Lists;

   protected body Safe_Object is
      procedure Put (M    : in Message'Class;
                     Meta : in Message_Metadata) is
      begin
         Append (Messages, M);
         Append (Metas, Meta);
      end Put;

      function Get_First return Message'Class is
      begin
         return First_Element (Messages);
      end Get_First;

      function Get_First_Metadata return Message_Metadata is
      begin
         return First_Element (Metas);
      end Get_First_Metadata;

      function Is_Empty return Boolean is
      begin
         return Is_Empty (Messages);
      end Is_Empty;

      procedure Remove_First is
      begin
         Delete_First (Messages);
         Delete_First (Metas);

         pragma Assert (Length (Messages) = Length (Metas));
      end Remove_First;

      entry Remove_First_Blocking
        (M     : out Handles.Message_Handle;
         Meta  : out Handles.Metadata_Handle)
        when not Messages.Is_Empty is
      begin
         M.Set (Messages.First_Element);
         Meta.Set (Metas.First_Element);
         Remove_First;
      end Remove_First_Blocking;

      procedure Remove_First_Non_Blocking
        (M     : out Handles.Message_Handle;
         Meta  : out Handles.Metadata_Handle)
      is
      begin
         if not Messages.Is_Empty then
            M.Set (Messages.First_Element);
            Meta.Set (Metas.First_Element);
            Remove_First;
         end if;
      end Remove_First_Non_Blocking;

   end Safe_Object;

end Sancta.Network.Inbox;
