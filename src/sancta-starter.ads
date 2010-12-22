package Sancta.Starter is

   --   pragma Preelaborate;
   Log_Section : constant String := "sancta.starter";

   procedure Launch;
   --  Id and Config file taken from command line.

   procedure Launch (Id         :     Node_Id;
                     Config_Xml :     String);
   --  Id and Config as supplied.
   --  May be called many times with several ids
   --   for several nodes in the same process.

   procedure Shutdown;

   procedure Shutdown_Abort (Code : Integer);
   pragma Precondition (Code /= 0);

end Sancta.Starter;
