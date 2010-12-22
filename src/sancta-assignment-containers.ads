 

with Agpl.Containers.Bulk;

package Sancta.Assignment.Containers is new
Agpl.Containers.Bulk (Assignment.Object,
                      "=",
                      Positive,
                      String,
                      "<",
                      Dummy_Key);

pragma Preelaborate (Sancta.Assignment.Containers);
