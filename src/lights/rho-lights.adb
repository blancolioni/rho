with Rho.Shaders.Slices.Attributes;
with Rho.Shaders.Slices.Main;
with Rho.Shaders.Slices.Uniforms;

package body Rho.Lights is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Light     : in out Root_Light_Type'Class;
      Color     : Rho.Color.Color_Type;
      Intensity :        Unit_Real := 1.0)
   is
   begin
      Light.Color := Color;
      Light.Intensity := Intensity;
   end Initialize;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Light : in out Root_Light_Type;
      Target : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Rho.Nodes.Root_Node_Type (Light).Load (Target);

      Target.Add_Shader
        (Target.Assets.Shader
           ("rho-shaders-light"));

      Light.Add_Slice
        (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
           (Fragment_Shader, "model", "mat4"));
      Light.Add_Slice
        (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
           (Fragment_Shader, "cameraPosition", "vec3"));
      Light.Add_Slice
        (Rho.Shaders.Slices.Attributes.Out_Attribute_Fragment
           (Vertex_Shader, "fragmentNormal", "vec3"));
      Light.Add_Slice
        (Rho.Shaders.Slices.Attributes.Out_Attribute_Fragment
           (Vertex_Shader, "fragmentVertex", "vec3"));
      Light.Add_Slice
        (Rho.Shaders.Slices.Attributes.In_Attribute_Fragment
           (Fragment_Shader, "fragmentNormal", "vec3"));
      Light.Add_Slice
        (Rho.Shaders.Slices.Attributes.In_Attribute_Fragment
           (Fragment_Shader, "fragmentVertex", "vec3"));

      for Slice of Root_Light_Type'Class (Light).Shader_Slices loop
         Target.Add_Shader_Fragment (Slice);
      end loop;

      Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Main.Shader_Line
           (Stage    => Vertex_Shader,
            Priority => 30,
            Name     => "copy vertex position to fragment shader",
            Line     => "fragmentVertex = position"));
      Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Main.Shader_Line
           (Stage    => Vertex_Shader,
            Priority => 30,
            Name     => "copy vertex normal to fragment shader",
            Line     => "fragmentNormal = vertexNormal"));

   end Load;

end Rho.Lights;
