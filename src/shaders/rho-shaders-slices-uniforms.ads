package Rho.Shaders.Slices.Uniforms is

   function Uniform_Fragment
     (Stage     : Shader_Stage;
      Name      : String;
      Type_Name : String)
      return Slice_Type;

end Rho.Shaders.Slices.Uniforms;
