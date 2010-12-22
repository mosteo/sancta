with Agpl.Conversions;
with Agpl.Strings.Fields;
with Agpl; use Agpl;

package body Sancta.Criteria is

   function Img is new Conversions.Decimal_To_Str (Costs);

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Criterion : in Assignment_Criteria;
                      Minmax    : in Costs;
                      Minsum    : in Costs;
                      Minavg    : in Costs) return Costs
   is
   begin
      if (Minmax = Infinite and then Criterion.Minmax_Weight > 0.0) or else
        (Minsum = Infinite and then Criterion.Minsum_Weight > 0.0) or else
        (Minavg = Infinite and then Criterion.Minavg_Weight > 0.0)
      then
         return Infinite;
      else
         return
           Costs (Criterion.Minmax_Weight) * Minmax +
           Costs (Criterion.Minsum_Weight) * Minsum +
           Costs (Criterion.Minavg_Weight) * Minavg;
      end if;
   end Evaluate;

   -----------
   -- Image --
   -----------

   function Image (C : in Costs) return String is
   begin
      return Costs'Image (C);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (C : in Costs; Decimals : in Natural) return String is
   begin
      return Img (C, Decimals);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (S : in String) return Assignment_Criteria is
      use Agpl.Strings.Fields;
   begin
      return
        (Float'Value (String_Head (S, ' ')),
         Float'Value (Select_Field (S, 2, ' ')),
         Float'Value (Select_Field (S, 3, ' ')));
   exception
      when others =>
         raise Constraint_Error with "@Sancta.Criteria.Value (S [mm, ms, ma])";
   end Value;

end Sancta.Criteria;
