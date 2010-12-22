 

--  Massive instantiation of containers

with Agpl.Containers.Bulk;

package Sancta.Agent.Containers is
new Agpl.Containers.Bulk (Object'Class,
                          "=",
                          Positive,
                          String, "<",
                          Get_Name2);

pragma Preelaborate (Sancta.Agent.Containers);
