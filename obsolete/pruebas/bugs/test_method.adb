procedure Test_Method is

   type X is tagged null record;

   procedure Void (This : in out X) is null;

   Y : X;

begin
--   Y.Void;   -- Fails with "no selector "Void" for type "X" defined at line 3
--   Void (Y); -- Compiles OK.

      --   Incidentally, the indexing of GPS gets botched by the use of
      --   "is null" in the procedure definition: it adds a level.
      --   This comment starts where GPS puts it, but is a level too deep.
end Test_Method;
