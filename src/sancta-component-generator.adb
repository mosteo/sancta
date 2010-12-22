package body Sancta.Component.Generator is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object_Initer) is
   begin
      This.Parent.Output
        (Provides_Value,
         This.Parent.To_Data (This.Parent.Option (Option_Value)));
   end Initialize;

end Sancta.Component.Generator;
