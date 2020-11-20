with Tau.Environment;

with Rho.Objects;
with Rho.Shaders;
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

   procedure Compile_Shader
     (Container : in out Root_Asset_Container_Type;
      Shader    : Rho.Shaders.Shader_Type)
   is abstract;

   function Create_Program
     (Container : in out Root_Asset_Container_Type;
      Name      : String;
      Shaders   : Rho.Shaders.Shader_Array)
      return Rho.Shaders.Program_Type
      is abstract;

   function Texture
     (Container : in out Root_Asset_Container_Type;
      Name      : String)
      return Rho.Textures.Texture_Type;

   type Asset_Container_Type is access all Root_Asset_Container_Type'Class;

private

   type Root_Asset_Container_Type is
     abstract new Rho.Objects.Root_Object_Type with
      record
         null;
      end record;

   overriding function Class_Name
     (Container : Root_Asset_Container_Type)
      return String
   is ("asset-container");

end Rho.Assets;
