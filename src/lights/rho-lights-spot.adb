with Rho.Logging;
with Rho.Matrices;
with Rho.Shaders.Slices.Main;
with Rho.Shaders.Slices.Uniforms;
with Rho.Values;

package body Rho.Lights.Spot is

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Light : in out Root_Spot_Light;
      Target : not null access Rho.Render.Render_Target'Class)
   is
      use Rho.Shaders.Slices;

      procedure Main_Line (Line : String);

      Priority : Shader_Source_Priority := 11;

      ---------------
      -- Main_Line --
      ---------------

      procedure Main_Line (Line : String) is
      begin
         Rho.Logging.Log
           ("raw: " & Line);

         Target.Add_Shader_Fragment
           (Rho.Shaders.Slices.Main.Shader_Line
              (Stage    => Fragment_Shader,
               Priority => Priority,
               Name     => "",
               Line     => Line));
         Priority := Priority + 1;
      end Main_Line;

   begin
      Root_Light_Type (Light).Load (Target);

      Target.Add_Shader
        (Target.Assets.Shader
           ("rho-shaders-light-spot"));

      Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
           (Fragment_Shader, "spotPosition", "vec3"));
      Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
           (Fragment_Shader, "spotColor", "vec3"));
      Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
           (Fragment_Shader, "attenuation", "float"));
      --  Target.Add_Shader_Fragment
      --    (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
      --       (Fragment_Shader, "spotIntensity", "float"));

      Main_Line ("vec3 normal = "
                 & "normalize (transpose (inverse (mat3 (model))) "
                 & "* fragmentNormal)");
      Main_Line ("vec3 surfacePosition = "
                 & "vec3(model * vec4(fragmentVertex, 1))");
      Main_Line ("vec3 surfaceToLight = "
                 & "normalize(spotPosition - surfacePosition)");
      Main_Line ("vec3 surfaceToCamera = "
                 & "normalize(cameraPosition - surfacePosition)");
      Main_Line ("float diffuseCoefficient = "
                 & "max (0.0, dot (normal, surfaceToLight))");
      Main_Line ("vec3 diffuse = "
                 & "diffuseCoefficient * fragmentColor.rgb "
                 & "* spotColor");
      Main_Line ("float distanceToLight = "
                 & "length (spotPosition - surfacePosition)");
      Main_Line ("float attenuation = "
                 & "1.0 / (1.0 + "
                 & "spotDecay * pow(distanceToLight, 2))");
      Main_Line ("vec3 linearColor = attenuation * diffuse");
      Main_Line ("vec3 gamma = vec3(1.0/2.2)");
      Main_Line ("fragmentColor = "
                 & "vec4(pow(linearColor, gamma), fragmentColor.a)");

      Target.Add_Uniform
        (Name  => "spotPosition",
         Value =>
           Rho.Values.Vector_Value (Light.Position));
      Target.Add_Uniform
        (Name  => "spotColor",
         Value =>
           Rho.Values.Vector_Value
             (Rho.Matrices.To_Vector
                  (Light.Color.R, Light.Color.G, Light.Color.B)));
      Target.Add_Uniform
        (Name  => "attenuation",
         Value =>
           Rho.Values.Real_Value (Light.Decay));
      --  Target.Add_Uniform
      --    (Name  => "spotIntensity",
      --     Value =>
      --       Rho.Values.Real_Value (Light.Intensity));
   end Load;

   ----------------
   -- Spot_Light --
   ----------------

   function Spot_Light
     (Color         : Rho.Color.Color_Type   := Rho.Color.White;
      Intensity     : Unit_Real := 1.0;
      Maximum_Range : Non_Negative_Real := 0.0;
      Dispersion    : Rho.Trigonometry.Angle :=
        Rho.Trigonometry.From_Degrees (60.0);
      Penumbra      : Unit_Real := 0.0;
      Decay         : Non_Negative_Real := 1.0)
      return Spot_Light_Type
   is

      Light : constant Spot_Light_Type :=
                new Root_Spot_Light'
                  (Rho.Nodes.Root_Node_Type with
                   Color         => Color,
                   Intensity     => Intensity,
                   Maximum_Range => Maximum_Range,
                   Dispersion    => Dispersion,
                   Penumbra      => Penumbra,
                   Decay         => Decay);
   begin
      return Light;
   end Spot_Light;

end Rho.Lights.Spot;
