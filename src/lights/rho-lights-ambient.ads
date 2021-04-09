private with Rho.Shaders.Slices.Uniforms;

package Rho.Lights.Ambient is

   type Root_Ambient_Light is
     new Root_Light_Type with private;

   type Ambient_Light_Type is access all Root_Ambient_Light'Class;

   function Ambient_Light
     (Color     : Rho.Color.Color_Type;
      Intensity : Unit_Real := 1.0)
      return Ambient_Light_Type;

private

   type Root_Ambient_Light is
     new Root_Light_Type with
      record
         null;
      end record;

   overriding function Shader_Slices
     (Light : Root_Ambient_Light)
      return Rho.Shaders.Slices.Slice_Array
   is ((1 => Rho.Shaders.Slices.Uniforms.Uniform_Fragment
          (Fragment_Shader, "ambientLightColor", "vec3")));

end Rho.Lights.Ambient;
