package body Rho.Shaders is

   ------------------------------------
   -- Standard_Binding_Element_Count --
   ------------------------------------

   function Standard_Binding_Element_Count
     (Binding : Standard_Variable_Binding) return Positive
   is
   begin
      return (case Binding is
                 when Position_Attribute             => 3,
                 when Vertex_Normal_Attribute        => 3,
                 when Vertex_Texture_Coord_Attribute => 2,
                 when others                         => 1);
   end Standard_Binding_Element_Count;

   ---------------------------
   -- Standard_Binding_Name --
   ---------------------------

   function Standard_Binding_Name
     (Binding : Standard_Variable_Binding) return String
   is
   begin
      return (case Binding is
              when Model_Uniform => "model",
              when View_Uniform => "camera",
              when Texture_Uniform => "tex",
              when Camera_Position_Uniform => "cameraPosition",
              when Ambient_Color_Uniform => "ambientColor",
              when Ambient_Coefficient_Uniform => "ambientCoefficient",
              when Spot_Position_Uniform => "spotPosition",
              when Spot_Color_Uniform => "spotColor",
              when Spot_Attenuation_Uniform => "attenuation",
              when Spot_Coefficient_Uniform => "coefficient",
              when Position_Attribute => "position",
              when Vertex_Normal_Attribute => "vertexNormal",
              when Vertex_Texture_Coord_Attribute => "texCoord");
   end Standard_Binding_Name;

end Rho.Shaders;
