with Agpl.Graphs.Bellman_Ford;

procedure Bellman_Ford_Test is
   package Test is new Agpl.Graphs.Bellman_Ford (Integer);
begin
   Test.Test_Package;
end Bellman_Ford_Test;
