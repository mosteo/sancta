package Player.Position2d.Exposed is

   --  Pragma Preelaborate;

   --  We use this package to get a view of the Position2d struct

private

   type Position2d_Struct is new Internal_Type;

   --  pragma Convention (C, Internal_Type);

end Player.Position2d.Exposed;
