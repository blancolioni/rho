with Rho.Color;
with Rho.Lights.Ambient;
with Rho.Lights.Spot;
with Rho.Material.Simple;

with Rho.Demos.Rotating_Cube;

package body Rho.Demos.Spotlight_Demo is

   type Cube_Demo_Type is
     new Rho.Demos.Rotating_Cube.Rotating_Cube_Demo with
      record
         Ambient   : Rho.Lights.Ambient.Ambient_Light_Type;
         Spotlight : Rho.Lights.Spot.Spot_Light_Type;
      end record;

   overriding function Category
     (Demo : Cube_Demo_Type)
      return String
   is ("lighting");

   overriding function Name
     (Demo : Cube_Demo_Type)
      return String
   is ("spotlight");

   overriding function Description
     (Demo : Cube_Demo_Type)
      return String
   is ("A rotating cube with a spotlight");

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
      Side : constant Rho.Material.Material_Type :=
               Rho.Material.Simple.Create_Simple_Material
                 (Handle.Assets.Create_Texture_From_Image
                    ("config/images/grass_block_side"));
      Top  : constant Rho.Material.Material_Type :=
               Rho.Material.Simple.Create_Simple_Material
                 (Handle.Assets.Create_Texture_From_Image
                    ("config/images/grass_block_top"));
   begin
      Demo.Initialize (Handle, Window,
                       (Side, Side, Top, Top, Side, Side));
      Demo.Ambient :=
        Rho.Lights.Ambient.Ambient_Light
          (Color     => (0.2, 0.2, 0.2, 1.0),
           Intensity => 1.0);
      Demo.Spotlight :=
        Rho.Lights.Spot.Spot_Light
          (Color         => Rho.Color.White,
           Intensity     => 1.0,
           Decay         => 0.1);
      Demo.Spotlight.Set_Position (0.0, 4.0, 0.0);

      Demo.Scene.Add (Demo.Spotlight);
      Demo.Scene.Add (Demo.Ambient);

      Demo.Start;
   end Execute;

   ----------
   -- Load --
   ----------

   function Load return Demo_Type is
   begin
      return new Cube_Demo_Type;
   end Load;

end Rho.Demos.Spotlight_Demo;
