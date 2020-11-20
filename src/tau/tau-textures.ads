with Rho.Textures;

with Tau.Objects;

package Tau.Textures is

   type Root_Tau_Texture is
     new Tau.Objects.Root_Tau_Object with private;

   type Tau_Texture is access all Root_Tau_Texture'Class;

   function Instantiate
     (Texture : Root_Tau_Texture)
      return Rho.Textures.Texture_Type;

private

   type Root_Tau_Texture is
     new Tau.Objects.Root_Tau_Object with
      record
         null;
      end record;

   overriding function Class_Name
     (Texture : Root_Tau_Texture)
      return String
   is ("texture");

end Tau.Textures;
