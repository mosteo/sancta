with Sancta.Debug;

package body Sancta.Tasks.Explore_Segment is

   ------------
   -- Create --
   ------------

   function Create (From, To : in Types.Pose) return Object is
   begin
      return (Sancta.Tasks.Compound.Object with
              P1 => From,
              P2 => To);
   end Create;

   --------------
   -- Get_From --
   --------------

   function Get_From (This : in Object) return Types.Pose is
   begin
      return This.P1;
   end Get_From;

   ------------
   -- Get_To --
   ------------

   function Get_To (This : in Object) return Types.Pose is
   begin
      return This.P2;
   end Get_To;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Object) return String is
   begin
      return "Explore segment " & Debug.To_String (This.P1) & " <-?-> " &
                                  Debug.To_String (This.P2);
   end To_String;

end Sancta.Tasks.Explore_Segment;
