 

with Sancta.Gap.Safe_Array;
with Agpl.Smart_Access_Limited;
pragma Elaborate_All (Agpl.Smart_Access_Limited);

package Sancta.Gap.Safe_Array_Handle is new
Agpl.Smart_Access_Limited (Sancta.Gap.Safe_Array.Object,
                           Sancta.Gap.Safe_Array.Object_Access);

pragma Preelaborate (Sancta.Gap.Safe_Array_Handle);
