with Ada.Containers.Indefinite_Ordered_Maps;

with Sancta.Config;
with Sancta.Options; use Sancta.Options;

--  Use this package to get instances of registered plugins

package body Sancta.Component.Factory is

   ---------------
   -- Is_Master --
   ---------------

   function Is_Master (This : Creator_On_Demand) return Boolean is
      pragma Unreferenced (This);
   begin
      return True;
   end Is_Master;

   --------------------
   -- Requires_Mutex --
   --------------------

   function Requires_Mutex (This : Creator_On_Demand) return Boolean is
      pragma Unreferenced (This);
   begin
      return True;
   end Requires_Mutex;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Creator_On_Demand;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      if This.Context.Keys.Is_Empty then
         Next := Clock + 365.0 * 24.0 * 60.0 * 60.0;
         return;
      end if;

      if This.Timer.Elapsed > Options.Max_Component_On_Demand_Wait_Period.Value then
         for I in This.Context.Keys.First_Index .. This.Context.Keys.Last_Index loop
            Log ("[master:" & (+This.Context.Name) &
                 "] still waiting for key [" &
                 String (This.Context.Keys.Element (I)) & "]",
                 Warning, Log_Section);
         end loop;
         This.Timer.Reset;
      end if;
      This.Period.Next (Next);
   end Run;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Creator_On_Demand;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      pragma Unreferenced (Value);
   --  Once all keys exist, we can create the surrogate plugin!
   begin
      Log ("[master:" & (+This.Context.Name) &
           "] unsubscribing from key [" & String (Key) & "]",
           Debug, Log_Section);

      This.Unsubscribe (Key);

      declare
         I : Internal_Key_Vectors.Cursor := This.Context.Keys.Find (Key);
      begin
         if Internal_Key_Vectors.Has_Element (I) then
            This.Context.Keys.Delete (I);
         else
            Log ("Key not found for deletion, this shouldn't happen...",
                 Error, Log_Section);
            if This.Context.Keys.Is_Empty then
               Log ("J:EMPTY", Error, Log_Section);
            end if;
            for J in This.Context.Keys.First_Index .. This.Context.Keys.Last_Index loop
               Log ("J:" & J'Img & " V:" & String (This.Context.Keys.Element (J)),
                    Error, Log_Section);
            end loop;
            return;
         end if;
      end;

      if This.Context.Keys.Is_Empty then -- No more keys to wait for, create!
         Config.Create_Plugin ("surrogate:" & This.Name.all, This.Config);
         Log ("[surrogate:" & This.Name.all &
              "] created successfully after " & This.Epoch.Image,
              Informative, Log_Section);
      end if;
   end Key_Stored;

   ----------------------
   -- Create_On_Demand --
   ----------------------

   function Create_On_Demand
     (Config  : Comp_Config;
      Env     : Environment.Object;
      Context : Context_On_Demand) return Object_Access
   is
      This : constant On_Demand_Access :=
        new Creator_On_Demand (new String'(+Context.Name), Config);
   begin
      Log ("Creating [master:" & (+Context.Name) & "]",
           Debug, Log_Section);
      This.Context := Context;
      This.Context.Keys := Context.Keys;
      --  There's some fscking bug around here messing with this. But not mine
      This.Env     := Env;
      This.Period.Set_Period (0.1);
      for I in Context.Keys.First_Index .. Context.Keys.Last_Index loop
         This.Subscribe (Context.Keys.Element (I));
      end loop;
      return Object_Access (This);
   end Create_On_Demand;

   type Kinds is (Simple, Plus, On_Demand);

   type Internal_Creator (Kind : Kinds) is record
      case Kind is
         when Simple =>
            Simple_Creator : Object_Creator;
         when Plus =>
            Plus_Creator   : Object_Creator_Plus;
         when On_Demand =>
            On_Demand_Creator : Context_On_Demand;
      end case;
   end record;

   package Creator_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Internal_Creator);

   use Creator_Maps;

   Creators : Map;

   ------------
   -- Create --
   ------------

   function Create (Name   : in String;
                    Config : in Agpl.Xml.Node;
                    Env    : in Environment.Object) return Object_Access
   is
      I : constant Cursor := Find (Creators, Name);
   begin
      if Has_Element (I) then
         declare
            Creator : constant Internal_Creator := Element (I);
         begin
            case Creator.Kind is
               when Simple =>
                  return Element (I).Simple_Creator (Config);
               when Plus =>
                  return Element (I).Plus_Creator (Config, Env);
               when On_Demand =>
                  return Create_On_Demand
                    (Config, Env, Element (I).On_Demand_Creator);
            end case;
         end;
      else
         Log ("Requested creation of unregistered component: " & Name, Error);
         raise Unregistered_Plugin
           with "Unregistered plugin: [" & Name & "]";
      end if;
   end Create;

   --------------
   -- Register --
   --------------

   procedure Register (Name : in String; Creator : in Object_Creator) is
   begin
      Creators.Insert (Name, (Simple, Creator));
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register (Name    : Component_Name;
                       Creator : Object_Creator_Plus) is
   begin
      Creators.Insert (Name, (Plus, Creator));
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register (Name     : Component_Name;
                       Creator  : Object_Creator_Plus;
                       Required : Internal_Key_Array)
   is
   begin
      Creators.Insert
        (Name,
         (On_Demand,
          (Name => +Name,
           Real => Creator,
           Keys => To_Vector (Required))));
      Register ("surrogate:" & Name, Creator);
   end Register;

end Sancta.Component.Factory;
