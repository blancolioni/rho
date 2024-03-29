with Rho.Lights.Ambient;
with Rho.Material.Basic;
with Rho.Material.Simple;

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
      Side : constant Rho.Material.Reference :=
               Rho.Material.Simple.Create
                 (Handle.Assets.Create_Texture_From_Image
                    ("config/images/grass_block_side"));
      Top  : constant Rho.Material.Reference :=
               Rho.Material.Basic.Create
                 (Handle.Assets.Create_Texture_From_Image
                    ("config/images/grass_block_top"))
        with Unreferenced;
   begin
      Demo.Initialize (Handle, Window, Side);
      Demo.Scene.Add
        (Rho.Lights.Ambient.Ambient_Light
           (Color     => (1.0, 1.0, 1.0, 1.0),
            Intensity => 1.0));
      --  (Side, Side, Side, Side, Top, Top));
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
