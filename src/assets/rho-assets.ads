private with WL.String_Maps;

with Rho.Objects;
with Rho.Textures;

package Rho.Assets is

   type Root_Asset_Container_Type is
     abstract new Rho.Objects.Root_Object_Type
   with private;

   overriding function Class_Name
     (Container : Root_Asset_Container_Type)
      return String;

   function Find_File
     (Container : Root_Asset_Container_Type;
      Name      : String;
      Extension : String)
      return String;

   function Generator_Name
     (Container : Root_Asset_Container_Type)
      return String
      is abstract;

--     function Shader
--       (Container : in out Root_Asset_Container_Type;
--        Name      : String)
--        return Rho.Shaders.Shader_Type;

   function Texture
     (Container : in out Root_Asset_Container_Type;
      Name      : String)
      return Rho.Textures.Texture_Type;

   function Create_Texture_From_Image
     (Container  : in out Root_Asset_Container_Type;
      Identifier : String)
      return Rho.Textures.Texture_Type
      is abstract;

   type Asset_Container_Type is access all Root_Asset_Container_Type'Class;

private

   package Identifier_Path_Maps is
     new WL.String_Maps (String);

   type Root_Asset_Container_Type is
     abstract new Rho.Objects.Root_Object_Type with
      record
         Map : Identifier_Path_Maps.Map;
      end record;

   overriding function Class_Name
     (Container : Root_Asset_Container_Type)
      return String
   is ("asset-container");

end Rho.Assets;
