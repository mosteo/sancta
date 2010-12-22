with Sancta.Types;

package Sancta.Constants is

   pragma Preelaborate;

   Agent_Colors : constant array (1 .. 99) of Types.Colors :=
                    (1      => (255,   0,   0),
                     2      => (  0,   0, 255),
                     3      => (  0, 255,   0),
                     4      => (  0, 255, 255),
                     5      => (255, 255,   0),
                     6      => (255,   0, 255),
                     7      => (165,  42,  42),
                     8      => (255, 165,   0),
                     9      => (  0, 100,   0),
                     others => (  0,   0,   0));

end Sancta.Constants;
