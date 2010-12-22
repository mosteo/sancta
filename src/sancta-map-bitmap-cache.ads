with Sancta.Map.Bitmap.Store;

with Agpl.Generic_Limited_File_Store.Cache;
pragma Elaborate_All (Agpl.Generic_Limited_File_Store.Cache);

package Sancta.Map.Bitmap.Cache is
new Sancta.Map.Bitmap.Store.Cache (Object, Hash);
