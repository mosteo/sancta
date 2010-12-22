--  Massive instantiation of containers

with Agpl.Containers.Bulk;

package Sancta.Tasks.Containers is
new Agpl.Containers.Bulk (Object'Class,
                          Same_Id,
                          Positive,
                          Task_Id, "<",
                          Tasks.Get_Id2);

pragma Preelaborate (Sancta.Tasks.Containers);
