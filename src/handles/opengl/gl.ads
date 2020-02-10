with System.Storage_Elements;
with Interfaces.C.Strings;
with GL_Types; use GL_Types;

package GL is

   procedure Attach_Shader
     (Program : Uint;
      Shader  : Uint);

   procedure Bind_Buffer
     (Target : GLenum;
      Buffer : Uint);

   procedure Bind_Texture
     (Target  : GLenum;
      Texture : Uint);

   procedure Bind_Vertex_Array
     (Array_Id : Uint);

   procedure Blend_Func
     (Sfactor : GLenum;
      Dfactor : GLenum);

   procedure Buffer_Data
     (Target : GLenum;
      Size   : Sizeiptr;
      Data   : System.Address;
      Usage  : GLenum);

   procedure Clear
     (Mask : Bit_Mask);

   procedure Clear_Color
     (Red   : GLfloat;
      Green : GLfloat;
      Blue  : GLfloat;
      Alpha : GLfloat);

   procedure Clear_Depth
     (Depth : GLfloat);

   procedure Compile_Shader
     (Shader : Uint);

   function Create_Program
     return Uint;

   function Create_Shader
     (Shader_Type : GLenum)
     return Uint;

   procedure Delete_Shader
     (Shader : Uint);

   procedure Depth_Function
     (Fn : GLenum);

   procedure Disable
     (Capability : GLenum);

   procedure Disable_Vertex_Attribute_Array
     (Index : Uint);

   procedure Draw_Elements
     (Mode       : GLenum;
      Count      : Sizei;
      Index_Type : GLenum;
      Offset     : System.Storage_Elements.Storage_Count);

   procedure Draw_Arrays
     (Mode  : GLenum;
      First : Int;
      Count : Sizei);

   procedure Draw_Arrays_Instanced
     (Mode            : GLenum;
      First           : Int;
      Count           : Sizei;
      Primitive_Count : Sizei);

   procedure Enable
     (Capability : GLenum);

   procedure Enable_Vertex_Attribute_Array
     (Index : Uint);

   procedure Gen_Buffers
     (Size    : Sizei;
      Buffers : access Uint);

   procedure Gen_Textures
     (Size     : Sizei;
      Textures : access Uint);

   procedure Gen_Vertex_Arrays
     (Size      : Sizei;
      Array_Ids : access Uint);

   function Get_Error
     return GLenum;

   procedure Get_Program
     (Program : Uint;
      Pname   : GLenum;
      Params  : access Int);

   procedure Get_Program_Info_Log
     (Program    : Uint;
      Max_Length : Sizei;
      Length     : access Sizei;
      Info_Log   : Interfaces.C.Strings.chars_ptr);

   function Get_Compile_Status
     (Shader : Uint)
      return Int;

   pragma Import (C, Get_Compile_Status, "Rho_get_compile_status");

   procedure Get_Shader
     (Shader : Uint;
      Pname  : GLenum;
      Params : access Int);

   procedure Get_Shader_Info_Log
     (Shader     : Uint;
      Max_Length : Sizei;
      Length     : access Sizei;
      Info_Log   : Interfaces.C.Strings.chars_ptr);

   function Get_String
     (Name : GLenum)
     return Interfaces.C.Strings.chars_ptr;

   function Get_Uniform_Location
     (Program : Uint;
      Name    : String)
     return Int;

   function Get_Attribute_Location
     (Program : Uint;
      Name    : String)
      return Int;

   procedure Link_Program
     (Program : Uint);

   procedure Polygon_Mode
     (Face : GLenum;
      Mode : GLenum);

   procedure Shader_Source
     (Shader : Uint;
      Count  : Sizei;
      Source : String);

   procedure Tex_Image_2D
     (Target          : GLenum;
      Level           : Int;
      Internal_Format : Int;
      Width           : Sizei;
      Height          : Sizei;
      Border          : Int;
      Format          : GLenum;
      Ptype           : GLenum;
      Pixels          : System.Address);

   procedure Tex_Parameter
     (Target          : GLenum;
      Parameter_Name  : GLenum;
      Parameter_Value : GLenum);

   procedure Tex_Parameter
     (Target          : GLenum;
      Parameter_Name  : GLenum;
      Parameter_Value : access GLfloat);

   procedure Uniform_Matrix
     (Location  : Uint;
      Count     : Sizei;
      Transpose : GLboolean;
      Matrix    : Array_Of_Float);

   procedure Uniform
     (Location : Uint;
      Value    : Int);

   procedure Uniform
     (Location : Uint;
      Value    : GLfloat);

   procedure Uniform
     (Location : Uint;
      Value    : Array_Of_Float);

   procedure Uniform_Int_Array
     (Location : Uint;
      Value    : Array_Of_Int);

   procedure Uniform_Float_Array
     (Location : Uint;
      Value    : Array_Of_Float);

   procedure Uniform_Float_Vector_3_Array
     (Location : Uint;
      Value    : Array_Of_Float);

   procedure Use_Program
     (Program : Uint);

   procedure Vertex_Attribute_Pointer
     (Index        : Uint;
      Size         : Int;
      Element_Type : GLenum;
      Normalized   : GLboolean;
      Stride       : Sizei;
      Pointer      : System.Storage_Elements.Storage_Offset);

   procedure Vertex_Attribute_Divisor
     (Index        : Uint;
      Divisor      : Uint);

   procedure Viewport
     (X : Int;
      Y : Int;
      W : Sizei;
      H : Sizei);

   procedure Disable_Debug;
   procedure Enable_Debug;
   function Debug_Enabled return Boolean;

end GL;
