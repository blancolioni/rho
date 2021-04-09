package Rho.Shaders.Slices.Main is

   function Shader_Line
     (Stage     : Shader_Stage;
      Priority  : Shader_Source_Priority;
      Name      : String;
      Line      : String)
      return Slice_Type;

end Rho.Shaders.Slices.Main;
