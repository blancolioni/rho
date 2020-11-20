private with Ada.Strings.Unbounded;

with Rho.Objects;

package Rho.Textures is

   type Root_Texture_Type is
     new Rho.Objects.Root_Object_Type with private;

   type Texture_Type is access all Root_Texture_Type'Class;

   function Dimension_Count
     (Texture : Root_Texture_Type'Class)
      return Positive;

   function Create_From_Image_Name
     (Texture_Name : String;
      Image_Name   : String;
      Format       : String;
      Order        : Positive)
      return Texture_Type
     with Pre => Order <= 3;

private

   type Root_Texture_Type is
     new Rho.Objects.Root_Object_Type with
      record
         Image_Name   : Ada.Strings.Unbounded.Unbounded_String;
         Image_Format : Ada.Strings.Unbounded.Unbounded_String;
         Dimensions   : Positive := 2;
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
