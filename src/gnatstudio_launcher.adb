with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Strings.Fixed;

procedure GNATStudio_Launcher is

   function Append_If_Exists (PATH_Name : String) return String;
   procedure Save_And_Set (Name, Val : String);
   function Append_If_Exists (PATH_Name : String) return String is
   begin
      if Exists (PATH_Name) then
         return ':' & Value (PATH_Name);
      else
         return "";
      end if;
   end Append_If_Exists;
   procedure Save_And_Set (Name, Val : String) is
      Backup_Name : constant String := "GPS_STARTUP_" & Name;
   begin
      if Exists (Name) then
         Set (Backup_Name, Value (Name));
      else
         Set (Backup_Name, "");
      end if;
      Set (Name, Val);
   end Save_And_Set;

   AL              : Argument_List (1 .. Argument_Count);
   Launcher_Ending : constant String := "_launcher";
   App_Loc         : constant String := "/MacOS";
   Launcher_Loc    : constant String := Command_Name;
   Exe_Loc         : constant String :=
     Launcher_Loc
       (Launcher_Loc'First .. Launcher_Loc'Last - Launcher_Ending'Length);
   Res_Loc : constant String :=
     Exe_Loc
       (Exe_Loc'First .. Ada.Strings.Fixed.Index (Exe_Loc, App_Loc) - 1) &
     "/Resources";

begin
   Put_Line ("Launcher: " & Launcher_Loc);
   Put_Line ("Exe     : " & Exe_Loc);
   Put_Line ("Res     : " & Res_Loc);
   Put_Line ("GS_GNAT_PATH: " & Append_If_Exists ("GS_GNAT_PATH"));
   Put_Line ("GS_GPR_PATH : " & Append_If_Exists ("GS_GPR_PATH"));

   Set ("GPS_ROOT", Res_Loc);
   Set ("GNATSTUDIO_PYTHONHOME", Res_Loc);
   Save_And_Set ("XDG_DATA_DIRS", Res_Loc & "/share");
   Save_And_Set ("GTK_EXE_PREFIX", Res_Loc);
   Save_And_Set
     ("GDK_PIXBUF_MODULE_FILE",
      Res_Loc & "/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache");
   Save_And_Set ("GI_TYPELIB_PATH", Res_Loc & "/lib/girepository-1.0");
   Set
     ("PATH",
      Value ("PATH") & Append_If_Exists ("GS_GNAT_PATH") &
      Append_If_Exists ("GS_GPR_PATH"));
   Set ("DYLD_LIBRARY_PATH", Res_Loc & "/lib");

   for Ind in AL'Range loop
      AL (Ind) := new String'(Argument (Ind));
      Put_Line ("Arg" & Ind'Image & ": " & AL (Ind).all);
   end loop;

   Put_Line ("Spawn: " & Spawn (Exe_Loc, AL)'Image);
end GNATStudio_Launcher;
