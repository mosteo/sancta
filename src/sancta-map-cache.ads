with Sancta.Map.Store;

with Agpl.Generic_Limited_File_Store.Cache;
pragma Elaborate_All (Agpl.Generic_Limited_File_Store.Cache);

package Sancta.Map.Cache is
new Sancta.Map.Store.Cache (Object'Class, Hash);
