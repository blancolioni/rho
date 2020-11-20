with Rho.Material.Basic;

with Rho.Demos.Rotating_Cube;

package body Rho.Demos.Texture_Demo is

   type Cube_Demo_Type is
     new Rho.Demos.Rotating_Cube.Rotating_Cube_Demo with
      record
         null;
      end record;

   overriding function Category
     (Demo : Cube_Demo_Type)
      return String
   is ("material");

   overriding function Name
     (Demo : Cube_Demo_Type)
      return String
   is ("basic_texture");

   overriding function Description
     (Demo : Cube_Demo_Type)
      return String
   is ("A simple rotating textured cube");

   overriding procedure Execute
     (Demo   : not null access Cube_Demo_Type;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Demo   : not null access Cube_Demo_Type;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type)
   is
      Material : constant Rho.Material.Material_Type :=
        Rho.Material.Basic.Create_Basic_Material
          (Handle.Assets.Texture
             ("rho-textures-ada_mascot"));
   begin
      Demo.Initialize (Handle, Window, Material);
      Demo.Start;
   end Execute;

   ----------
   -- Load --
   ----------

   function Load return Demo_Type is
   begin
      return new Cube_Demo_Type;
   end Load;

end Rho.Demos.Texture_Demo;
