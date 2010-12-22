with Agpl.Generic_Messenger;

--  Second kind of update messages. This is used for network GUI updating.
--  This is due to my lack of foresight, should be the last variation!!

package Sancta.Gui.Robot_Data.Messages2 is
new Agpl.Generic_Messenger (Network_Update_Kinds, Network_Update);

--  pragma Preelaborate (Sancta.Gui.Robot_Data.Messages2);
