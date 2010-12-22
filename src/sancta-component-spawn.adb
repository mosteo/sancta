with Agpl.Os_Utils,
     Sancta.Component.Factory;

package body Sancta.Component.Spawn is

   task type Exec is
      entry Spawn (Command : String);
      entry Done;
   end Exec;

   task body Exec is
      Full_Command : access String;
      Code         :        Integer;
   begin
      accept Spawn (Command : String) do
         Full_Command := new String'(Command);
      end Spawn;

      Code := Agpl.Os_Utils.Spawn (Full_Command.all);

      if Code /= 0 then
         Log ("Exit code:" & Code'Img, warning, Log_Section);
      else
         Log ("Finished.", Debug, Log_Section);
      end if;

      select
         accept Done;
      or
         terminate;
      end select;
   end Exec;

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
     (Config : in Comp_Config)
      return Component.Object_Access
   is
      This    : constant Root.Object_Access := new Object (Name'Access, Config);
      Command : constant String := This.Option (Option_Command);
      Ex      : constant access Exec := new Exec;
   begin
      Log ("Executing: " & Command, Debug, Log_Section);

      if This.Exists (Option_Info) then
         Log ("Spawn info: " & This.Option (Option_Info),
              Always, Log_Section);
      end if;

      Ex.Spawn (Command);

      if Boolean'Value (This.Option (Option_Wait, Default_Wait'Img)) then
         Ex.Done;
      end if;

      return Component.Object_Access (This);
   end Create;

end Sancta.Component.Spawn;
