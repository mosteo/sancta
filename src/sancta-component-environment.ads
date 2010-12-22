with Agpl.Containers.String_String_Maps;

package Sancta.Component.Environment is

   type Object is tagged record
      Id        : Node_Id;
      Constants : Agpl.Containers.String_String_Maps.Map;
   end record;

end Sancta.Component.Environment;
