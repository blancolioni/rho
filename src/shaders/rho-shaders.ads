package Rho.Shaders with Pure is

   type Variable_Mode is (In_Variable, Out_Variable, Uniform_Variable);

   type Variable_Binding_Type is
     (No_Standard_Binding,
      Model_Uniform,
      View_Uniform,
      Texture_Uniform,
      Camera_Position_Uniform,
      Ambient_Color_Uniform,
      Ambient_Coefficient_Uniform,
      Spot_Position_Uniform,
      Spot_Color_Uniform,
      Spot_Attenuation_Uniform,
      Spot_Coefficient_Uniform,
      Position_Attribute,
      Vertex_Normal_Attribute,
      Vertex_Texture_Coord_Attribute);

   subtype Standard_Variable_Binding is
     Variable_Binding_Type range Model_Uniform .. Variable_Binding_Type'Last;

   subtype Standard_Uniform_Binding is
     Variable_Binding_Type range Model_Uniform .. Spot_Coefficient_Uniform;

   subtype Standard_Attribute_Binding is
     Variable_Binding_Type range
       Position_Attribute .. Vertex_Texture_Coord_Attribute;

   function Standard_Binding_Name
     (Binding : Standard_Variable_Binding)
      return String;

   function Standard_Binding_Element_Count
     (Binding : Standard_Variable_Binding)
      return Positive;

end Rho.Shaders;
