function Sancta.Debug.Parse_Montesano
  (Filename : in String;
   Rps      : in Natural)
   return        Types.Posed_Range_Scan_Vectors.Vector;

--  File format is:

--  A free-format line with information. The repeats until EOF:

--  distance readings of laser
--  followed by odometric pose at time of scan.
