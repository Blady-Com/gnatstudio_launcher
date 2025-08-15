with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;

procedure GNATStudio_Launcher is

   package Var_RC is new Ada.Containers.Indefinite_Ordered_Maps (String, String);
   Var_RC_map : Var_RC.Map;
   use type Var_RC.Cursor;

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

   procedure Parse_Var_RC is
      GSLRC       : constant String := "/.gnatstudio/gnatstudio_launcher.rc";
      Var_RC_File : File_Type;
      Found       : Boolean         := False;
   begin
      if Exists ("GNATSTUDIO_HOME") then
         if Ada.Directories.Exists (Value ("GNATSTUDIO_HOME") & GSLRC) then
            Put_Line ("Found: " & Value ("GNATSTUDIO_HOME") & GSLRC);
            Open (Var_RC_File, In_File, Value ("GNATSTUDIO_HOME") & GSLRC);
            Found := True;
         end if;
      elsif Exists ("GPS_HOME") then
         if Ada.Directories.Exists (Value ("GPS_HOME") & GSLRC) then
            Put_Line ("Found: " & Value ("GPS_HOME") & GSLRC);
            Open (Var_RC_File, In_File, Value ("GPS_HOME") & GSLRC);
            Found := True;
         end if;
      elsif Exists ("HOME") then
         if Ada.Directories.Exists (Value ("HOME") & GSLRC) then
            Put_Line ("Found: " & Value ("HOME") & GSLRC);
            Open (Var_RC_File, In_File, Value ("HOME") & GSLRC);
            Found := True;
         end if;
      end if;
      if Found then
         while not End_Of_File (Var_RC_File) loop
            declare
               Line : constant String := Get_Line (Var_RC_File);
               use Ada.Strings.Fixed;
            begin
               if Line'Length /= 0 and then Line (1) /= '#' then
                  if Index (Line, "=") /= 0 then
                     Var_RC_map.Insert (Line (1 .. Index (Line, "=") - 1), Line (Index (Line, "=") + 1 .. Line'Last));
                     Put_Line
                       ("Found: " & Line (1 .. Index (Line, "=") - 1) & '=' &
                        Line (Index (Line, "=") + 1 .. Line'Last));
                  end if;
               end if;
            end;
         end loop;
         Close (Var_RC_File);
      else
         Put_Line ("No gnatstudio_launcher.rc file found.");
      end if;
   end Parse_Var_RC;

   procedure Set_Var_RC is
      use Var_RC;
   begin
      for C in Var_RC_map.Iterate loop
         if Exists (Key (C)) then
            Put_Line ("Append " & Key (C) & " with: " & Element (C));
            Set (Key (C), Value (Key (C)) & ':' & Element (C));
         else
            Put_Line ("Create " & Key (C) & " with: " & Element (C));
            Set (Key (C), Element (C));
         end if;
      end loop;
   end Set_Var_RC;

   AL              : Argument_List (1 .. Argument_Count);
   Launcher_Ending : constant String := "_launcher";
   App_Loc         : constant String := "/MacOS";
   Launcher_Loc    : constant String := Command_Name;
   Exe_Loc         : constant String := Launcher_Loc (Launcher_Loc'First .. Launcher_Loc'Last - Launcher_Ending'Length);
   Res_Loc         : constant String :=
     Exe_Loc (Exe_Loc'First .. Ada.Strings.Fixed.Index (Exe_Loc, App_Loc) - 1) & "/Resources";

begin
   Put_Line ("Launcher: " & Launcher_Loc);
   Put_Line ("Exe     : " & Exe_Loc);
   Put_Line ("Res     : " & Res_Loc);
   Put_Line ("GS_GNAT_PATH: " & Append_If_Exists ("GS_GNAT_PATH"));
   Put_Line ("GS_GPR_PATH : " & Append_If_Exists ("GS_GPR_PATH"));
   Parse_Var_RC;

   Set ("GPS_ROOT", Res_Loc);
   Set ("GNATSTUDIO_PYTHONHOME", Res_Loc);
   Save_And_Set ("XDG_DATA_DIRS", Res_Loc & "/share");
   Save_And_Set ("GTK_EXE_PREFIX", Res_Loc);
   Save_And_Set ("GDK_PIXBUF_MODULE_FILE", Res_Loc & "/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache");
   Save_And_Set ("GI_TYPELIB_PATH", Res_Loc & "/lib/girepository-1.0");
   if Var_RC_map.Find ("PATH") = Var_RC.No_Element then
      Put_Line ("Append PATH with: " & Append_If_Exists ("GS_GNAT_PATH") & Append_If_Exists ("GS_GPR_PATH"));
      Set ("PATH", Value ("PATH") & Append_If_Exists ("GS_GNAT_PATH") & Append_If_Exists ("GS_GPR_PATH"));
   end if;
   Set ("DYLD_FALLBACK_LIBRARY_PATH", "/usr/lib:" & Res_Loc & "/lib:" & Res_Loc & "/lib/gdk-pixbuf-2.0/2.10.0/loaders");
   Set_Var_RC;

   for Ind in AL'Range loop
      AL (Ind) := new String'(Argument (Ind));
      Put_Line ("Arg" & Ind'Image & ": " & AL (Ind).all);
   end loop;

   Put_Line ("Spawn: " & Spawn (Exe_Loc, AL)'Image);
end GNATStudio_Launcher;
