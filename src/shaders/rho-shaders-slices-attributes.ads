package Rho.Shaders.Slices.Attributes is

   function In_Attribute_Fragment
     (Stage     : Shader_Stage;
      Name      : String;
      Type_Name : String)
      return Slice_Type;

   function Out_Attribute_Fragment
     (Stage     : Shader_Stage;
      Name      : String;
      Type_Name : String)
      return Slice_Type;

end Rho.Shaders.Slices.Attributes;
