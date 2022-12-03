private with Ada.Strings.Unbounded;

with Rho.Objects;

with Rho.Shaders.Slices;
with Rho.Render;

package Rho.Textures is

   Max_Texture_Dimensions : constant := 3;
   type Texture_Dimension_Count is range 0 .. Max_Texture_Dimensions;
   subtype Texture_Dimension_Index is
     Texture_Dimension_Count range 1 .. Texture_Dimension_Count'Last;

   type Texture_Address_Mode is (Border, Clamp, Mirror, Wrap);
   type Texture_Filter_Type is (Nearest, Linear);

   type Root_Texture_Type is
     abstract new Rho.Objects.Root_Object_Type
     and Rho.Shaders.Slices.Slice_Container_Interface
   with private;

   type Texture_Type is access all Root_Texture_Type'Class;

   overriding function Class_Name
     (Texture : Root_Texture_Type)
      return String
   is ("texture");

   procedure Load
     (Texture : in out Root_Texture_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is abstract;

   procedure Unload (Texture : in out Root_Texture_Type;
                     Target   : not null access Rho.Render.Render_Target'Class)
   is abstract;

   procedure Activate
     (Texture : Root_Texture_Type;
      Target  : not null access Rho.Render.Render_Target'Class)
   is abstract;

   overriding function Shader_Slices
     (Texture : Root_Texture_Type)
      return Rho.Shaders.Slices.Slice_Array;

   overriding procedure Add_Slice
     (Texture : in out Root_Texture_Type;
      Slice : Rho.Shaders.Slices.Slice_Type);

   function Dimension_Count
     (Texture : Root_Texture_Type'Class)
      return Texture_Dimension_Count;

   function S_Border
     (Texture : Root_Texture_Type'Class)
     return Texture_Address_Mode;

   function T_Border
     (Texture : Root_Texture_Type'Class)
     return Texture_Address_Mode;

   function Mag_Filter
     (Texture : Root_Texture_Type'Class)
      return Texture_Filter_Type;

   procedure Initialize
     (Texture    : in out Root_Texture_Type'Class;
      Identifier : String;
      Order      : Texture_Dimension_Count;
      Width      : Positive;
      Height     : Positive;
      Depth      : Positive := 1;
      S_Border   : Texture_Address_Mode := Wrap;
      T_Border   : Texture_Address_Mode := Wrap;
      Mag_Filter : Texture_Filter_Type  := Linear);

private

   type Root_Texture_Type is
     abstract new Rho.Objects.Root_Object_Type
     and Rho.Shaders.Slices.Slice_Container_Interface with
      record
         Identifier   : Ada.Strings.Unbounded.Unbounded_String;
         Dimensions   : Texture_Dimension_Count := 2;
         Slices       : Rho.Shaders.Slices.Slice_Container;
         Width        : Positive;
         Height       : Positive;
         Depth        : Positive;
         S_Border     : Texture_Address_Mode;
         T_Border     : Texture_Address_Mode;
         Mag_Filter   : Texture_Filter_Type;
      end record;

   overriding function Shader_Slices
     (Texture : Root_Texture_Type)
      return Rho.Shaders.Slices.Slice_Array
   is (Texture.Slices.Shader_Slices);

   function Dimension_Count
     (Texture : Root_Texture_Type'Class)
      return Texture_Dimension_Count
   is (Texture.Dimensions);

   function S_Border
     (Texture : Root_Texture_Type'Class)
      return Texture_Address_Mode
   is (Texture.S_Border);

   function T_Border
     (Texture : Root_Texture_Type'Class)
     return Texture_Address_Mode
   is (Texture.T_Border);

   function Mag_Filter
     (Texture : Root_Texture_Type'Class)
      return Texture_Filter_Type
   is (Texture.Mag_Filter);

end Rho.Textures;
