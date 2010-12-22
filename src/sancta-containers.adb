package body Sancta.Containers is

   ------------
   -- Bundle --
   ------------

   function Bundle
     (Addr : Network.Address;
      Msg  : Network.Message'Class)
      return Msg_Bundle
   is
      use Network.Handles;
   begin
      return (Set (Addr), Network.Set (Msg));
   end Bundle;

end Sancta.Containers;
