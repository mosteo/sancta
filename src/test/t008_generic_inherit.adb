procedure T008_Generic_Inherit is

   package Base_Package is

      type Base is abstract tagged null record;

      procedure X (B : Base) is abstract;

      type Base_Int is limited interface;

      procedure Y (Bi : Base_Int) is abstract;

   end Base_Package;

   generic
      type Child is new Base_Package.Base with private;
      type Child_Int is new Base_Package.Base_Int with private;
   package Child_Package is

      type Grand_Child is new Child with null record;

      overriding
      procedure X (Gc : Grand_Child) is null;

      type Grand_Child_Int is new Child_Int with null record;

      overriding
      procedure Y (Gci : Grand_Child_Int) is null;

   end Child_Package;

begin
   null;
end T008_Generic_Inherit;
