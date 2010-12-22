with Agpl.Generic_Limited_File_Store;
pragma Elaborate_All (Agpl.Generic_Limited_File_Store);

package Sancta.Map.Store is
new Agpl.Generic_Limited_File_Store (Object'Class,
                                     Object'Class'Write,
                                     Object'Class'Read);
