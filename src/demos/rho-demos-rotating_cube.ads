with Rho.Material;

package Rho.Demos.Rotating_Cube is

   type Rotating_Cube_Demo is
     abstract new Root_Demo_Type with private;

   function Scene (Demo : Rotating_Cube_Demo) return Rho.Scenes.Scene_Type;

   procedure Initialize
     (Demo     : not null access Rotating_Cube_Demo'Class;
      Handle   : Rho.Handles.Handle;
      Window   : Rho.Windows.Window_Type;
      Material : Rho.Material.Reference);

   procedure Initialize
     (Demo     : not null access Rotating_Cube_Demo'Class;
      Handle   : Rho.Handles.Handle;
      Window   : Rho.Windows.Window_Type;
      Material : Rho.Material.Reference_Array);

   procedure Start
     (Demo     : not null access Rotating_Cube_Demo'Class);

private

   type Rotating_Cube_Demo is
     abstract new Root_Demo_Type with
      record
         Handle   : Rho.Handles.Handle;
         Window   : Rho.Windows.Window_Type;
         Scene    : Rho.Scenes.Scene_Type;
      end record;

   function Scene (Demo : Rotating_Cube_Demo) return Rho.Scenes.Scene_Type
   is (Demo.Scene);

end Rho.Demos.Rotating_Cube;
