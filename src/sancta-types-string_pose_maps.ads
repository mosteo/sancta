with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Sancta.Types.String_Pose_Maps is new
Ada.Containers.Indefinite_Hashed_Maps (String, Pose,
                                       Ada.Strings.Hash,
                                       "=");

pragma Preelaborate (Sancta.Types.String_Pose_Maps);
