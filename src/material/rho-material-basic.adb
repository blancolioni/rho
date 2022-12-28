package body Rho.Material.Basic is

   ------------
   -- Create --
   ------------

   function Create
     (Color : Rho.Color.Color_Type)
      return Reference
   is
   begin
      return Material : constant Reference := new Instance do
         Material.Default_Shaders;
         Material.Shader_Names.Include ("single-color");
         Material.Static_Bindings.Append
           (Static_Binding'
              (Rho.Values.Color_Value,
               Ada.Strings.Unbounded.To_Unbounded_String ("surfaceColor"),
               Rho.Values.Color_Value (Color)));
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Texture : not null access
        Rho.Textures.Root_Texture_Type'Class)
      return Reference
   is
   begin
      return Material : constant Reference := new Instance do
         Material.Default_Shaders;
         Material.Shader_Names.Include ("textured");
         Material.Textures.Append (Rho.Textures.Texture_Type (Texture));
      end return;
   end Create;

end Rho.Material.Basic;
