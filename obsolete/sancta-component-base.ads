with Sancta.Component.Root;

package Sancta.Component.Base is

   --  Retrieval of configuration options
   type Object is new Root.Object with null record;

   function Get_Name (This : Object) return Component_Name;

   function Option is new Agpl.Xml.Get_Generic_Value
     (Object,
      Option_Attr,
      Integer,
      Get_Config,
      Image,
      Integer'Image,
      Integer'Value);

--     function Option (This : Object;
--                      Attr : Option_Attr;
--                      Def  : Integer) return Integer;

end Sancta.Component.Base;
