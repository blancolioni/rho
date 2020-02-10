with Rho.Material.Depth;

with Rho.Demos.Rotating_Cube;

package body Rho.Demos.Depth_Demo is

   type Depth_Demo_Type is
     new Rho.Demos.Rotating_Cube.Rotating_Cube_Demo with
      record
         null;
      end record;

   overriding function Category
     (Demo : Depth_Demo_Type)
      return String
   is ("material");

   overriding function Name
     (Demo : Depth_Demo_Type)
      return String
   is ("depth");

   overriding function Description
     (Demo : Depth_Demo_Type)
      return String
   is ("Depth material demo");

   overriding procedure Execute
     (Demo   : not null access Depth_Demo_Type;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Demo   : not null access Depth_Demo_Type;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type)
   is
      Material : constant Rho.Material.Material_Type :=
        Rho.Material.Depth.Create_Depth_Material
          (Near => 4.0,
           Far  => 8.0);
   begin
      Demo.Initialize (Handle, Window, Material);
      Demo.Start;
   end Execute;

   ----------
   -- Load --
   ----------

   function Load return Demo_Type is
   begin
      return new Depth_Demo_Type;
   end Load;

end Rho.Demos.Depth_Demo;
