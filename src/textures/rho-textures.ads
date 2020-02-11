with Rho.Objects;

package Rho.Textures is

   type Root_Texture_Type is
     new Rho.Objects.Root_Object_Type with private;

   type Texture_Type is access all Root_Texture_Type'Class;

   function Dimension_Count
     (Texture : Root_Texture_Type'Class)
      return Positive;

private

   type Root_Texture_Type is
     new Rho.Objects.Root_Object_Type with
      record
         Dimensions : Positive := 2;
      end record;

   overriding function Class_Name
     (Texture : Root_Texture_Type)
      return String
   is ("texture");

   function Dimension_Count
     (Texture : Root_Texture_Type'Class)
      return Positive
   is (Texture.Dimensions);

end Rho.Textures;
