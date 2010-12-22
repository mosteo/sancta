 

--  To simulate several traderbots interacting.

with Sancta.Draw_Mtsp;

-------------------
-- Draw_Solution --
-------------------

procedure Sancta.Traderbot_Sim.Draw_Solution (This : in Object) is
begin
   Draw_Mtsp.From_Tasks (This.Ass.Get_Agents);
end Sancta.Traderbot_Sim.Draw_Solution;
