with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with WL.Command_Line;

with Tau.Library;

with Rho.Handles.OpenGL;
with Rho.Handles.Simulation;

with Rho.Windows;

with Rho.Demos;

with Rho.Options;
with Rho.Paths;

procedure Rho.Driver is
begin

   if not Ada.Directories.Exists (".rho-options") then
      Ada.Directories.Copy_File
        (Source_Name => Rho.Paths.Config_File ("default-options.txt"),
         Target_Name => ".rho-options");
   end if;

   WL.Command_Line.Load_Defaults (".rho-options");

   Tau.Library.Load_Library;

   Rho.Demos.Load_Demos;

   if Rho.Options.List then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Available demos:");

      declare

         Longest_Name : Natural := 0;

         procedure Measure_Name (Demo : Rho.Demos.Demo_Type);
         procedure Put_Demo (Demo : Rho.Demos.Demo_Type);

         ------------------
         -- Measure_Name --
         ------------------

         procedure Measure_Name (Demo : Rho.Demos.Demo_Type) is
            Name : constant String := Demo.Category & "/" & Demo.Name;
         begin
            Longest_Name := Natural'Max (Longest_Name, Name'Length);
         end Measure_Name;

         --------------
         -- Put_Demo --
         --------------

         procedure Put_Demo (Demo : Rho.Demos.Demo_Type) is
            use Ada.Text_IO;
         begin
            Put (Standard_Error, Demo.Category & "/" & Demo.Name);
            Set_Col (Standard_Error, Ada.Text_IO.Count (Longest_Name + 4));
            Put_Line (Standard_Error, Demo.Description);
         end Put_Demo;

      begin
         Rho.Demos.Iterate_Demos (Measure_Name'Access);
         Rho.Demos.Iterate_Demos (Put_Demo'Access);
      end;

      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   declare
      Demo_Name : constant String := Rho.Options.Demo;
      Handle    : Rho.Handles.Handle;
   begin
      if not Rho.Demos.Exists (Demo_Name) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "unknown demo: " & Demo_Name & " (use --list to list all demos)");
         Ada.Command_Line.Set_Exit_Status (1);
         return;
      end if;

      if Rho.Options.Driver = "opengl" then
         Handle := Rho.Handles.OpenGL.Get_Handle;
      elsif Rho.Options.Driver = "simulation" then
         Handle := Rho.Handles.Simulation.Get_Handle;
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "unknown driver: " & Rho.Options.Driver);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "available drivers are: opengl, simulation");
         Ada.Command_Line.Set_Exit_Status (2);
         return;
      end if;

      declare
         Window : constant Rho.Windows.Window_Type :=
           Handle.Create_Window
             (0.0, 0.0, 640.0, 480.0, True);
         Demo   : constant Rho.Demos.Demo_Type :=
           Rho.Demos.Get (Demo_Name);
      begin
         Demo.Execute (Handle, Window);
      end;
   end;

end Rho.Driver;
