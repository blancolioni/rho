with Rho.Matrices;
with Rho.Shaders.Slices.Main;
with Rho.Shaders.Slices.Uniforms;
with Rho.Values;

package body Rho.Lights.Ambient is

   -------------------
   -- Ambient_Light --
   -------------------

   function Ambient_Light
     (Color     : Rho.Color.Color_Type;
      Intensity : Unit_Real := 1.0)
      return Ambient_Light_Type
   is
   begin
      return Light : constant Ambient_Light_Type :=
        new Root_Ambient_Light
      do
         Light.Initialize (Color, Intensity);
         Light.Add_Slice
           (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
              (Fragment_Shader, "ambientLightColor", "vec3"));
         Light.Add_Slice
           (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
              (Fragment_Shader, "ambientCoefficient", "float"));
         Light.Add_Slice
           (Rho.Shaders.Slices.Main.Shader_Line
              (Stage    => Fragment_Shader,
               Priority => 10,
               Name     => "adjust surface color with ambient light",
               Line     =>
                 "fragmentColor.rgb = "
               & "ambientCoefficient * fragmentColor.rgb"
               & " * ambientLightColor"));
      end return;
   end Ambient_Light;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Light : in out Root_Ambient_Light;
      Target : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Root_Light_Type (Light).Load (Target);
      Target.Add_Uniform
        (Name  => "ambientLightColor",
         Value =>
           Rho.Values.Vector_Value
             (Rho.Matrices.To_Vector
                  (Light.Color.R, Light.Color.G, Light.Color.B)));
      Target.Add_Uniform
        (Name  => "ambientCoefficient",
         Value =>
           Rho.Values.Real_Value (Light.Intensity));
   end Load;

end Rho.Lights.Ambient;
