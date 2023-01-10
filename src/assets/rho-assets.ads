private with WL.String_Maps;

with System.Storage_Elements;

with Rho.Formats;
with Rho.Loaders;
with Rho.Objects;
with Rho.Shaders.Partial;

with Rho.Fonts;
with Rho.Textures.Creator;

package Rho.Assets is

   type Root_Asset_Container_Type is
     abstract new Rho.Objects.Root_Object_Type
     and Rho.Loaders.Loader_Interface
     and Rho.Textures.Creator.Texture_Creator_Interface
   with private;

   type Asset_Container_Type is access all Root_Asset_Container_Type'Class;

   overriding function Class_Name
     (Container : Root_Asset_Container_Type)
      return String;

   overriding function Find_File
     (Container : Root_Asset_Container_Type;
      Name      : String;
      Extension : String)
      return String;

   function Generator_Name
     (Container : Root_Asset_Container_Type)
      return String
      is abstract;

   function Partial_Shader
     (Container : in out Root_Asset_Container_Type;
      Name      : String;
      Stage     : Shader_Stage)
      return Rho.Shaders.Partial.Partial_Shader_Type;

   --  function Texture
   --    (Container : in out Root_Asset_Container_Type;
   --     Name      : String)
   --     return Texture_Access;

   function Create_Texture_From_Image
     (Container  : in out Root_Asset_Container_Type;
      Identifier : String)
      return Rho.Textures.Texture_Type
      is abstract;

   function Create_Texture
     (Container     : in out Root_Asset_Container_Type;
      Image_Buffer  : System.Address;
      Buffer_Length : System.Storage_Elements.Storage_Count;
      Width, Height : Natural;
      Offset        : System.Storage_Elements.Storage_Offset;
      Stride        : System.Storage_Elements.Storage_Count;
      Format        : Rho.Formats.Image_Format;
      Flip_Vertical : Boolean)
      return Rho.Textures.Texture_Type
      is abstract;

   function Font
     (Container  : in out Root_Asset_Container_Type;
      Family     : String;
      Size       : String;
      Style      : String;
      Weight     : String)
      return Rho.Fonts.Reference
      is abstract;

private

   package Identifier_Path_Maps is
     new WL.String_Maps (String);

   type Root_Asset_Container_Type is
     abstract new Rho.Objects.Root_Object_Type
     and Rho.Loaders.Loader_Interface
     and Rho.Textures.Creator.Texture_Creator_Interface with
      record
         Map : Identifier_Path_Maps.Map;
      end record;

   overriding function Class_Name
     (Container : Root_Asset_Container_Type)
      return String
   is ("asset-container");

end Rho.Assets;
