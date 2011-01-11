procedure Test_Ambig is

   type X is (Data, Meta);

   procedure Foo (Data : in Natural) is
      Z : constant X := Meta;
   begin
      if Z = X'(Data) then
         null;
      end if;

      case Z is
--         when Data => null;
--         when X'(Data) => null;
--         when X.Data => null;
--         when X (Data) => null;
         when Meta => null;
      end case;
   end Foo;

begin
   null;
end Test_Ambig;
