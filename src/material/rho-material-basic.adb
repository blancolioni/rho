package body Rho.Material.Basic is

   ---------------------------
   -- Create_Basic_Material --
   ---------------------------

   function Create_Basic_Material
     (Color : Rho.Color.Color_Type)
      return Material_Type
   is
   begin
      return Material : constant Material_Type :=
        new Root_Material_Type
      do
         Material.Default_Shaders;
         Material.Shader_Names.Include ("single-color");
         Material.Static_Bindings.Append
           (Static_Binding'
              (Rho.Values.Color_Value,
               Ada.Strings.Unbounded.To_Unbounded_String ("surfaceColor"),
               Rho.Values.Color_Value (Color)));
      end return;
   end Create_Basic_Material;

   ---------------------------
   -- Create_Basic_Material --
   ---------------------------

   function Create_Basic_Material
     (Texture : not null access
        Rho.Textures.Root_Texture_Type'Class)
      return Material_Type
   is
   begin
      return Material : constant Material_Type :=
        new Root_Material_Type
      do
         Material.Default_Shaders;
         Material.Shader_Names.Include ("textured");
         Material.Textures.Append (Rho.Textures.Texture_Type (Texture));
      end return;
   end Create_Basic_Material;

end Rho.Material.Basic;
