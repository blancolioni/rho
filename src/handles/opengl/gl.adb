with Ada.Text_IO;
with Ada.Float_Text_IO;

with GL.Debug;
with GL_Errors;

package body GL is

   Debug_Active : Boolean := False;

   -------------------
   -- Attach_Shader --
   -------------------

   procedure Attach_Shader
     (Program : Uint;
      Shader  : Uint)
   is
      type Lib_glAttachShader is access
        procedure
          (Program : Uint;
           Shader  : Uint);
      pragma Convention (C, Lib_glAttachShader);
      glAttachShader : Lib_glAttachShader;
      pragma Import (C, glAttachShader,
                     "_ptrc_glAttachShader");
   begin
      glAttachShader
        (Program,
         Shader);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Attach_Shader;

   -----------------
   -- Bind_Buffer --
   -----------------

   procedure Bind_Buffer
     (Target : GLenum;
      Buffer : Uint)
   is
      type Lib_glBindBuffer is access
        procedure
          (Target : GLenum;
           Buffer : Uint);
      pragma Convention (C, Lib_glBindBuffer);
      glBindBuffer : Lib_glBindBuffer;
      pragma Import (C, glBindBuffer,
                     "_ptrc_glBindBuffer");
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glBindBuffer: " & GL.Debug.Image (Target) & Buffer'Img);
      end if;

      glBindBuffer
        (Target,
         Buffer);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Bind_Buffer;

   ------------------
   -- Bind_Texture --
   ------------------

   procedure Bind_Texture
     (Target  : GLenum;
      Texture : Uint)
   is
      type Lib_glBindTexture is access
        procedure
          (Target  : GLenum;
           Texture : Uint);
      pragma Convention (C, Lib_glBindTexture);
      glBindTexture : Lib_glBindTexture;
      pragma Import (C, glBindTexture,
                     "_ptrc_glBindTexture");
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glBindTexture: " & Debug.Image (Target) & Texture'Img);
      end if;

      glBindTexture
        (Target,
         Texture);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Bind_Texture;

   -----------------------
   -- Bind_Vertex_Array --
   -----------------------

   procedure Bind_Vertex_Array
     (Array_Id : Uint)
   is
      type Lib_glBindVertexArray is access
        procedure
          (Array_Id : Uint);
      pragma Convention (C, Lib_glBindVertexArray);
      glBindVertexArray : Lib_glBindVertexArray;
      pragma Import (C, glBindVertexArray,
                     "_ptrc_glBindVertexArray");
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glBindVertexArray: " & Array_Id'Img);
      end if;

      glBindVertexArray
        (Array_Id);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Bind_Vertex_Array;

   ----------------
   -- Blend_Func --
   ----------------

   procedure Blend_Func
     (Sfactor : GLenum;
      Dfactor : GLenum)
   is
      type Lib_glBlendFunc is access
        procedure
          (Sfactor : GLenum;
           Dfactor : GLenum);
      pragma Convention (C, Lib_glBlendFunc);
      glBlendFunc : Lib_glBlendFunc;
      pragma Import (C, glBlendFunc,
                     "_ptrc_glBlendFunc");
   begin
      glBlendFunc
        (Sfactor,
         Dfactor);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Blend_Func;

   -----------------
   -- Buffer_Data --
   -----------------

   procedure Buffer_Data
     (Target : GLenum;
      Size   : Sizeiptr;
      Data   : System.Address;
      Usage  : GLenum)
   is
      type Lib_glBufferData is access
        procedure
          (Target : GLenum;
           Size   : Sizeiptr;
           Data   : System.Address;
           Usage  : GLenum);
      pragma Convention (C, Lib_glBufferData);
      glBufferData : Lib_glBufferData;
      pragma Import (C, glBufferData,
                     "_ptrc_glBufferData");
      Error : GLenum;
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glBufferData: "
            & GL.Debug.Image (Target)
            & Size'Img
            & " " & GL.Debug.Image (Usage));
      end if;
      glBufferData
        (Target,
         Size,
         Data,
         Usage);
      Error := Get_Error;

      if Error /= 0 then
         raise Program_Error;
      end if;
   end Buffer_Data;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Mask : Bit_Mask)
   is
      type Lib_glClear is access
        procedure
          (Mask : Bit_Mask);
      pragma Convention (C, Lib_glClear);
      glClear : Lib_glClear;
      pragma Import (C, glClear,
                     "_ptrc_glClear");
   begin
      glClear
        (Mask);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Clear;

   -----------------
   -- Clear_Color --
   -----------------

   procedure Clear_Color
     (Red   : GLfloat;
      Green : GLfloat;
      Blue  : GLfloat;
      Alpha : GLfloat)
   is
      type Lib_glClearColor is access
        procedure
          (Red   : GLfloat;
           Green : GLfloat;
           Blue  : GLfloat;
           Alpha : GLfloat);
      pragma Convention (C, Lib_glClearColor);
      glClearColor : Lib_glClearColor;
      pragma Import (C, glClearColor,
                     "_ptrc_glClearColor");
   begin
      glClearColor
        (Red,
         Green,
         Blue,
         Alpha);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Clear_Color;

   -----------------
   -- Clear_Depth --
   -----------------

   procedure Clear_Depth
     (Depth : GLfloat)
   is
      type Lib_GlClearDepth is access
        procedure
          (Depth  : GLfloat);
      pragma Convention (C, Lib_GlClearDepth);
      GlClearDepth : Lib_GlClearDepth;
      pragma Import (C, GlClearDepth,
                     "_ptrc_glClearDepth");
   begin
      GlClearDepth
        (Depth);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Clear_Depth;

   --------------------
   -- Compile_Shader --
   --------------------

   procedure Compile_Shader
     (Shader : Uint)
   is
      type Lib_glCompileShader is access
        procedure
          (Shader : Uint);
      pragma Convention (C, Lib_glCompileShader);
      glCompileShader : Lib_glCompileShader;
      pragma Import (C, glCompileShader,
                     "_ptrc_glCompileShader");
   begin
      glCompileShader
        (Shader);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Compile_Shader;

   --------------------
   -- Create_Program --
   --------------------

   function Create_Program
     return Uint
   is
      type Lib_glCreateProgram is access
        function
          return Uint;
      pragma Convention (C, Lib_glCreateProgram);
      glCreateProgram : Lib_glCreateProgram;
      pragma Import (C, glCreateProgram,
                     "_ptrc_glCreateProgram");
   begin
      return glCreateProgram.all;
   end Create_Program;

   -------------------
   -- Create_Shader --
   -------------------

   function Create_Shader
     (Shader_Type : GLenum)
     return Uint
   is
      type Lib_glCreateShader is access
        function
          (Shader_Type : GLenum)
          return Uint;
      pragma Convention (C, Lib_glCreateShader);
      glCreateShader : Lib_glCreateShader;
      pragma Import (C, glCreateShader,
                     "_ptrc_glCreateShader");
   begin
      if glCreateShader = null then
         raise Constraint_Error with "glCreateShader not found";
      end if;

      return glCreateShader
        (Shader_Type);
   end Create_Shader;

   -------------------
   -- Debug_Enabled --
   -------------------

   function Debug_Enabled return Boolean is
   begin
      return Debug_Active;
   end Debug_Enabled;

   -------------------
   -- Delete_Shader --
   -------------------

   procedure Delete_Shader
     (Shader : Uint)
   is
      type Lib_glDeleteShader is access
        procedure
          (Shader : Uint);
      pragma Convention (C, Lib_glDeleteShader);
      glDeleteShader : Lib_glDeleteShader;
      pragma Import (C, glDeleteShader,
                     "_ptrc_glDeleteShader");
   begin
      glDeleteShader
        (Shader);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Delete_Shader;

   --------------------
   -- Depth_Function --
   --------------------

   procedure Depth_Function
     (Fn : GLenum)
   is
      type Lib_GLDepthFunc is access
        procedure
          (Capability : GLenum);
      pragma Convention (C, Lib_GLDepthFunc);
      GLDepthFunc : Lib_GLDepthFunc;
      pragma Import (C, GLDepthFunc,
                     "_ptrc_glDepthFunc");
   begin
      GLDepthFunc (Fn);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Depth_Function;

   -------------
   -- Disable --
   -------------

   procedure Disable
     (Capability : GLenum)
   is
      type Lib_glDisable is access
        procedure
          (Capability : GLenum);
      pragma Convention (C, Lib_glDisable);
      glDisable : Lib_glDisable;
      pragma Import (C, glDisable,
                     "_ptrc_glDisable");
   begin
      glDisable
        (Capability);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Disable;

   -------------------
   -- Disable_Debug --
   -------------------

   procedure Disable_Debug is
   begin
      Debug_Active := False;
   end Disable_Debug;

   ------------------------------------
   -- Disable_Vertex_Attribute_Array --
   ------------------------------------

   procedure Disable_Vertex_Attribute_Array
     (Index : Uint)
   is
      type Lib_glDisableVertexAttribArray is access
        procedure
          (Index : Uint);
      pragma Convention (C, Lib_glDisableVertexAttribArray);
      glDisableVertexAttribArray : Lib_glDisableVertexAttribArray;
      pragma Import (C, glDisableVertexAttribArray,
                     "_ptrc_glDisableVertexAttribArray");
   begin
      glDisableVertexAttribArray
        (Index);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Disable_Vertex_Attribute_Array;

   -----------------
   -- Draw_Arrays --
   -----------------

   procedure Draw_Arrays
     (Mode  : GLenum;
      First : Int;
      Count : Sizei)
   is
      type Lib_glDrawArrays is access
        procedure
          (Mode  : GLenum;
           First : Int;
           Count : Sizei);
      pragma Convention (C, Lib_glDrawArrays);
      glDrawArrays : Lib_glDrawArrays;
      pragma Import (C, glDrawArrays,
                     "_ptrc_glDrawArrays");
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glDrawArrays: "
            & GL.Debug.Image (Mode)
            & First'Img
            & Count'Img);
      end if;
      glDrawArrays
        (Mode,
         First,
         Count);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Draw_Arrays;

   ---------------------------
   -- Draw_Arrays_Instanced --
   ---------------------------

   procedure Draw_Arrays_Instanced
     (Mode            : GLenum;
      First           : Int;
      Count           : Sizei;
      Primitive_Count : Sizei)
   is
      procedure Rho_GlDrawArraysInstanced
        (Mode            : GLenum;
         First           : Int;
         Count           : Sizei;
         Primitive_Count : Sizei);
      pragma Import (C, Rho_GlDrawArraysInstanced,
                     "Rho_glDrawArraysInstanced");
      Error : GLenum;
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glDrawArraysInstanced: "
            & GL.Debug.Image (Mode)
            & First'Img
            & Count'Img
            & Primitive_Count'Img);
      end if;
      Rho_GlDrawArraysInstanced
        (Mode,
         First,
         Count,
         Primitive_Count);
      Error := Get_Error;
      if Error /= 0 then
         raise Program_Error with Error'Img;
      end if;
   end Draw_Arrays_Instanced;

   procedure Draw_Elements
     (Mode       : GLenum;
      Count      : Sizei;
      Index_Type : GLenum;
      Offset     : System.Storage_Elements.Storage_Count)
   is
      type Proc is access
        procedure
          (Mode       : GLenum;
           Count      : Sizei;
           Index_Type : GLenum;
           Offset     : System.Storage_Elements.Storage_Count);
      pragma Convention (C, Proc);
      glDrawElements : Proc;
      pragma Import (C, glDrawElements,
                     "_ptrc_glDrawElements");
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glDrawElements: "
            & GL.Debug.Image (Mode)
            & Count'Img
            & " " & GL.Debug.Image (Index_Type)
            & Offset'Image);
      end if;
      glDrawElements
        (Mode, Count, Index_Type, Offset);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Draw_Elements;

   ------------
   -- Enable --
   ------------

   procedure Enable
     (Capability : GLenum)
   is
      type Lib_glEnable is access
        procedure
          (Capability : GLenum);
      pragma Convention (C, Lib_glEnable);
      glEnable : Lib_glEnable;
      pragma Import (C, glEnable,
                     "_ptrc_glEnable");
   begin
      glEnable
        (Capability);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Enable;

   ------------------
   -- Enable_Debug --
   ------------------

   procedure Enable_Debug is
   begin
      Debug_Active := True;
   end Enable_Debug;

   -----------------------------------
   -- Enable_Vertex_Attribute_Array --
   -----------------------------------

   procedure Enable_Vertex_Attribute_Array
     (Index : Uint)
   is
      type Lib_glEnableVertexAttribArray is access
        procedure
          (Index : Uint);
      pragma Convention (C, Lib_glEnableVertexAttribArray);
      glEnableVertexAttribArray : Lib_glEnableVertexAttribArray;
      pragma Import (C, glEnableVertexAttribArray,
                     "_ptrc_glEnableVertexAttribArray");
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glEnableVertexAttribArray: " & Index'Img);
      end if;
      glEnableVertexAttribArray
        (Index);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Enable_Vertex_Attribute_Array;

   -----------------
   -- Gen_Buffers --
   -----------------

   procedure Gen_Buffers
     (Size    : Sizei;
      Buffers : access Uint)
   is
      type Lib_glGenBuffers is access
        procedure
          (Size    : Sizei;
           Buffers : access Uint);
      pragma Convention (C, Lib_glGenBuffers);
      glGenBuffers : Lib_glGenBuffers;
      pragma Import (C, glGenBuffers,
                     "_ptrc_glGenBuffers");
   begin
      glGenBuffers
        (Size,
         Buffers);

      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glGenBuffers" & Size'Img & " -> " & Buffers.all'Img);
      end if;
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Gen_Buffers;

   ------------------
   -- Gen_Textures --
   ------------------

   procedure Gen_Textures
     (Size     : Sizei;
      Textures : access Uint)
   is
      type Lib_glGenTextures is access
        procedure
          (Size     : Sizei;
           Textures : access Uint);
      pragma Convention (C, Lib_glGenTextures);
      glGenTextures : Lib_glGenTextures;
      pragma Import (C, glGenTextures,
                     "_ptrc_glGenTextures");
   begin
      glGenTextures
        (Size,
         Textures);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Gen_Textures;

   -----------------------
   -- Gen_Vertex_Arrays --
   -----------------------

   procedure Gen_Vertex_Arrays
     (Size      : Sizei;
      Array_Ids : access Uint)
   is
      type Lib_glGenVertexArrays is access
        procedure
          (Size     : Sizei;
           Textures : access Uint);
      pragma Convention (C, Lib_glGenVertexArrays);
      glGenVertexArrays : Lib_glGenVertexArrays;
      pragma Import (C, glGenVertexArrays,
                     "_ptrc_glGenVertexArrays");
   begin
      glGenVertexArrays
        (Size,
         Array_Ids);

      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glGenVertexArrays" & Size'Img & " -> " & Array_Ids.all'Img);
      end if;

      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Gen_Vertex_Arrays;

   ----------------------------
   -- Get_Attribute_Location --
   ----------------------------

   function Get_Attribute_Location
     (Program : Uint;
      Name    : String)
      return Int
   is
      type Lib_Proc is access
        function
          (Program : Uint;
           Name    : Interfaces.C.Strings.chars_ptr)
           return Int;
      pragma Convention (C, Lib_Proc);
      Proc : Lib_Proc;
      pragma Import (C, Proc,
                     "_ptrc_glGetAttribLocation");
      C_Name : Interfaces.C.Strings.chars_ptr :=
                 Interfaces.C.Strings.New_String (Name);
      Result : constant Int := Proc (Program, C_Name);
   begin
      Interfaces.C.Strings.Free (C_Name);
      return Result;
   end Get_Attribute_Location;

   ---------------
   -- Get_Error --
   ---------------

   function Get_Error
     return GLenum
   is
      type Lib_glGetError is access
        function
          return GLenum;
      pragma Convention (C, Lib_glGetError);
      glGetError : Lib_glGetError;
      pragma Import (C, glGetError,
                     "_ptrc_glGetError");
      Result : constant GLenum := glGetError.all;
   begin
      if Result /= 0 then
         GL_Errors.Put_Error (Result);
      end if;
      return Result;
   end Get_Error;

   -----------------
   -- Get_Program --
   -----------------

   procedure Get_Program
     (Program : Uint;
      Pname   : GLenum;
      Params  : access Int)
   is
      type Lib_glGetProgramiv is access
        procedure
          (Program : Uint;
           Pname   : GLenum;
           Params  : access Int);
      pragma Convention (C, Lib_glGetProgramiv);
      glGetProgramiv : Lib_glGetProgramiv;
      pragma Import (C, glGetProgramiv,
                     "_ptrc_glGetProgramiv");
   begin
      glGetProgramiv
        (Program,
         Pname,
         Params);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Get_Program;

   --------------------------
   -- Get_Program_Info_Log --
   --------------------------

   procedure Get_Program_Info_Log
     (Program    : Uint;
      Max_Length : Sizei;
      Length     : access Sizei;
      Info_Log   : Interfaces.C.Strings.chars_ptr)
   is
      type Lib_glGetProgramInfoLog is access
        procedure
          (Program    : Uint;
           Max_Length : Sizei;
           Length     : access Sizei;
           Info_Log   : Interfaces.C.Strings.chars_ptr);
      pragma Convention (C, Lib_glGetProgramInfoLog);
      glGetProgramInfoLog : Lib_glGetProgramInfoLog;
      pragma Import (C, glGetProgramInfoLog,
                     "_ptrc_glGetProgramInfoLog");
   begin
      glGetProgramInfoLog
        (Program,
         Max_Length,
         Length,
         Info_Log);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Get_Program_Info_Log;

   ----------------
   -- Get_Shader --
   ----------------

   procedure Get_Shader
     (Shader : Uint;
      Pname  : GLenum;
      Params : access Int)
   is
      type Lib_glGetShaderiv is access
        procedure
          (Shader : Uint;
           Pname  : GLenum;
           Params : access Int);
      pragma Convention (C, Lib_glGetShaderiv);
      glGetShaderiv : Lib_glGetShaderiv;
      pragma Import (C, glGetShaderiv,
                     "_ptrc_glGetShaderiv");
   begin
      glGetShaderiv
        (Shader,
         Pname,
         Params);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Get_Shader;

   -------------------------
   -- Get_Shader_Info_Log --
   -------------------------

   procedure Get_Shader_Info_Log
     (Shader     : Uint;
      Max_Length : Sizei;
      Length     : access Sizei;
      Info_Log   : Interfaces.C.Strings.chars_ptr)
   is
      type Lib_glGetShaderInfoLog is access
        procedure
          (Shader     : Uint;
           Max_Length : Sizei;
           Length     : access Sizei;
           Info_Log   : Interfaces.C.Strings.chars_ptr);
      pragma Convention (C, Lib_glGetShaderInfoLog);
      glGetShaderInfoLog : Lib_glGetShaderInfoLog;
      pragma Import (C, glGetShaderInfoLog,
                     "_ptrc_glGetShaderInfoLog");
      Error : GLenum;

   begin
      glGetShaderInfoLog
        (Shader,
         Max_Length,
         Length,
         Info_Log);
      Error := Get_Error;

      if Error /= 0 then
         raise Program_Error;
      end if;
   end Get_Shader_Info_Log;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (Name : GLenum)
     return Interfaces.C.Strings.chars_ptr
   is
      type Lib_glGetString is access
        function
          (Name : GLenum)
          return Interfaces.C.Strings.chars_ptr;
      pragma Convention (C, Lib_glGetString);
      glGetString : Lib_glGetString;
      pragma Import (C, glGetString,
                     "_ptrc_glGetString");
   begin
      return glGetString
        (Name);
   end Get_String;

   --------------------------
   -- Get_Uniform_Location --
   --------------------------

   function Get_Uniform_Location
     (Program : Uint;
      Name    : String)
     return Int
   is
      type Lib_glGetUniformLocation is access
        function
          (Program : Uint;
           Name    : Interfaces.C.Strings.chars_ptr)
          return Int;
      pragma Convention (C, Lib_glGetUniformLocation);
      glGetUniformLocation : Lib_glGetUniformLocation;
      pragma Import (C, glGetUniformLocation,
                     "_ptrc_glGetUniformLocation");
      C_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (Name);
      Result : constant Int := glGetUniformLocation (Program, C_Name);
   begin
      Interfaces.C.Strings.Free (C_Name);

      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glUniform" & Program'Img & " -> " & Result'Img);
      end if;

      return Result;

   end Get_Uniform_Location;

   ------------------
   -- Link_Program --
   ------------------

   procedure Link_Program
     (Program : Uint)
   is
      type Lib_glLinkProgram is access
        procedure
          (Program : Uint);
      pragma Convention (C, Lib_glLinkProgram);
      glLinkProgram : Lib_glLinkProgram;
      pragma Import (C, glLinkProgram,
                     "_ptrc_glLinkProgram");
   begin
      glLinkProgram
        (Program);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Link_Program;

   ------------------
   -- Polygon_Mode --
   ------------------

   procedure Polygon_Mode
     (Face : GLenum;
      Mode : GLenum)
   is
      type Lib_glPolygonMode is access
        procedure
          (Face : GLenum;
           Mode : GLenum);
      pragma Convention (C, Lib_glPolygonMode);
      glPolygonMode : Lib_glPolygonMode;
      pragma Import (C, glPolygonMode,
                     "_ptrc_glPolygonMode");
      Error : GLenum;
   begin
      glPolygonMode
        (Face,
         Mode);
      Error := Get_Error;
      if Error /= 0 then
         raise Program_Error;
      end if;
   end Polygon_Mode;

   -------------------
   -- Shader_Source --
   -------------------

   procedure Shader_Source
     (Shader : Uint;
      Count  : Sizei;
      Source : String)
   is
      pragma Unreferenced (Count);

      procedure Rho_Shader_Source
        (Shader : Uint;
         Source : Interfaces.C.Strings.chars_ptr);

      pragma Import (C, Rho_Shader_Source, "Rho_shader_source");

      C_Source : Interfaces.C.Strings.chars_ptr :=
                   Interfaces.C.Strings.New_String (Source);
      Error : GLenum;
   begin

      Rho_Shader_Source (Shader, C_Source);

      Error := Get_Error;

      Interfaces.C.Strings.Free (C_Source);

      if Error /= 0 then
         raise Program_Error with GL.Debug.Image (Error);
      end if;

   end Shader_Source;

   ------------------
   -- Tex_Image_2D --
   ------------------

   procedure Tex_Image_2D
     (Target          : GLenum;
      Level           : Int;
      Internal_Format : Int;
      Width           : Sizei;
      Height          : Sizei;
      Border          : Int;
      Format          : GLenum;
      Ptype           : GLenum;
      Pixels          : System.Address)
   is
      type Lib_glTeximage2D is access
        procedure
          (Target          : GLenum;
           Level           : Int;
           Internal_Format : Int;
           Width           : Sizei;
           Height          : Sizei;
           Border          : Int;
           Format          : GLenum;
           Ptype           : GLenum;
           Pixels          : System.Address);
      pragma Convention (C, Lib_glTeximage2D);
      glTeximage2D : Lib_glTeximage2D;
      pragma Import (C, glTeximage2D,
                     "_ptrc_glTexImage2D");
   begin
      glTeximage2D
        (Target,
         Level,
         Internal_Format,
         Width,
         Height,
         Border,
         Format,
         Ptype,
         Pixels);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Tex_Image_2D;

   -------------------
   -- Tex_Parameter --
   -------------------

   procedure Tex_Parameter
     (Target          : GLenum;
      Parameter_Name  : GLenum;
      Parameter_Value : GLenum)
   is
      type Lib_glTexParameteri is access
        procedure
          (Target          : GLenum;
           Parameter_Name  : GLenum;
           Parameter_Value : GLenum);
      pragma Convention (C, Lib_glTexParameteri);
      glTexParameteri : Lib_glTexParameteri;
      pragma Import (C, glTexParameteri,
                     "_ptrc_glTexParameteri");
   begin
      glTexParameteri
        (Target,
         Parameter_Name,
         Parameter_Value);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Tex_Parameter;

   -------------------
   -- Tex_Parameter --
   -------------------

   procedure Tex_Parameter
     (Target          : GLenum;
      Parameter_Name  : GLenum;
      Parameter_Value : access GLfloat)
   is
      type Lib_glTexParameterfv is access
        procedure
          (Target          : GLenum;
           Parameter_Name  : GLenum;
           Parameter_Value : access GLfloat);
      pragma Convention (C, Lib_glTexParameterfv);
      glTexParameterfv : Lib_glTexParameterfv;
      pragma Import (C, glTexParameterfv,
                     "_ptrc_glTexParameterfv");
   begin
      glTexParameterfv
        (Target,
         Parameter_Name,
         Parameter_Value);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Tex_Parameter;

   -------------
   -- Uniform --
   -------------

   procedure Uniform
     (Location : Uint;
      Value    : Int)
   is
      type Lib_Proc is access
        procedure (Location : Uint; V0 : Int);
      pragma Convention (C, Lib_Proc);
      Proc : Lib_Proc;
      pragma Import (C, Proc,
                     "_ptrc_glUniform1i");
      Error : GLenum;
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glUniform1i" & Location'Img & Value'Img);
      end if;
      Proc (Location, Value);
      Error := Get_Error;
      if Error /= 0 then
         raise Program_Error with GL.Debug.Image (Error);
      end if;
   end Uniform;

   -------------
   -- Uniform --
   -------------

   procedure Uniform
     (Location : Uint;
      Value    : GLfloat)
   is
      type Lib_1 is access
        procedure (Location : Uint;
                   Value_1  : GLfloat);
      pragma Convention (C, Lib_1);
      Proc_1 : Lib_1;
      pragma Import (C, Proc_1, "_ptrc_glUniform1f");
   begin
      if Debug_Active then
         Ada.Text_IO.Put
           ("glUniform1f" & Location'Img & " ");
         GL.Debug.Put (Value);
         Ada.Text_IO.New_Line;
      end if;

      Proc_1 (Location, Value);

   end Uniform;

   -------------
   -- Uniform --
   -------------

   procedure Uniform
     (Location : Uint;
      Value    : Array_Of_Float)
   is
      type Lib_1 is access
        procedure (Location : Uint;
                   Value_1  : GLfloat);
      type Lib_2 is access
        procedure (Location : Uint;
                   Value_1  : GLfloat;
                   Value_2  : GLfloat);
      type Lib_3 is access
        procedure (Location : Uint;
                   Value_1  : GLfloat;
                   Value_2  : GLfloat;
                   Value_3  : GLfloat);
      type Lib_4 is access
        procedure (Location : Uint;
                   Value_1  : GLfloat;
                   Value_2  : GLfloat;
                   Value_3  : GLfloat;
                   Value_4  : GLfloat);

      pragma Convention (C, Lib_1);
      pragma Convention (C, Lib_2);
      pragma Convention (C, Lib_3);
      pragma Convention (C, Lib_4);

      Proc_1 : Lib_1;
      Proc_2 : Lib_2;
      Proc_3 : Lib_3;
      Proc_4 : Lib_4;

      pragma Import (C, Proc_1, "_ptrc_glUniform1f");
      pragma Import (C, Proc_2, "_ptrc_glUniform2f");
      pragma Import (C, Proc_3, "_ptrc_glUniform3f");
      pragma Import (C, Proc_4, "_ptrc_glUniform4f");

      Error : GLenum;
   begin

      if Debug_Active then
         Ada.Text_IO.Put
           ("glUniform" & Integer'Image (Value'Length) (2)
            & "f" & Location'Img & " ");
         GL.Debug.Put (Value);
         Ada.Text_IO.New_Line;
      end if;

      case Value'Length is
         when 1 =>
            Proc_1 (Location, Value (Value'First));
         when 2 =>
            Proc_2 (Location, Value (Value'First), Value (Value'First + 1));
         when 3 =>
            Proc_3 (Location,
                    Value (Value'First),
                    Value (Value'First + 1),
                    Value (Value'First + 2));
         when 4 =>
            Proc_4 (Location,
                    Value (Value'First),
                    Value (Value'First + 1),
                    Value (Value'First + 2),
                    Value (Value'First + 3));
         when others =>
            raise Program_Error with
              "invalid argument to glUniform";
      end case;

      Error := Get_Error;
      if Error /= 0 then
         raise Program_Error with GL.Debug.Image (Error);
      end if;
   end Uniform;

   -------------------------
   -- Uniform_Float_Array --
   -------------------------

   procedure Uniform_Float_Array
     (Location : Uint;
      Value    : Array_Of_Float)
   is
      type Lib_1 is access
        procedure (Location : Uint;
                   Count    : Sizei;
                   Value    : access constant GLfloat);
      pragma Convention (C, Lib_1);
      Proc_1 : Lib_1;
      pragma Import (C, Proc_1, "_ptrc_glUniform1fv");
      Count : constant Sizei := Sizei (Value'Length);

      Error : GLenum;

   begin
      if Debug_Active then
         Ada.Text_IO.Put
           ("glUniform1fv" & Location'Img & " ");
         GL.Debug.Put (Value);
         Ada.Text_IO.New_Line;
      end if;

      Proc_1 (Location, Count, Value (Value'First)'Access);

      Error := Get_Error;
      if Error /= 0 then
         raise Program_Error with GL.Debug.Image (Error);
      end if;
   end Uniform_Float_Array;

   ----------------------------------
   -- Uniform_Float_Vector_3_Array --
   ----------------------------------

   procedure Uniform_Float_Vector_3_Array
     (Location : Uint;
      Value    : Array_Of_Float)
   is
      type Lib_1 is access
        procedure (Location : Uint;
                   Count    : Sizei;
                   Value    : access constant GLfloat);
      pragma Convention (C, Lib_1);
      Proc_1 : Lib_1;
      pragma Import (C, Proc_1, "_ptrc_glUniform3fv");
      Count : constant Sizei := Sizei (Value'Length) / 3;

      Error : GLenum;

   begin
      if Debug_Active then
         Ada.Text_IO.Put
           ("glUniform3fv" & Location'Img & " ");
         GL.Debug.Put (Value);
         Ada.Text_IO.New_Line;
      end if;

      Proc_1 (Location, Count, Value (Value'First)'Access);

      Error := Get_Error;
      if Error /= 0 then
         raise Program_Error with GL.Debug.Image (Error);
      end if;
   end Uniform_Float_Vector_3_Array;

   -----------------------
   -- Uniform_Int_Array --
   -----------------------

   procedure Uniform_Int_Array
     (Location : Uint;
      Value    : Array_Of_Int)
   is
      type Lib_1 is access
        procedure (Location : Uint;
                   Count    : Sizei;
                   Value    : access constant Int);
      pragma Convention (C, Lib_1);
      Proc_1 : Lib_1;
      pragma Import (C, Proc_1, "_ptrc_glUniform1iv");
      Count : constant Sizei := Sizei (Value'Length);

      Error : GLenum;
   begin
      if Debug_Active then
         Ada.Text_IO.Put
           ("glUniform1iv" & Location'Img & " ");
--         GL.Debug.Put (Value);
         Ada.Text_IO.New_Line;
      end if;

      Proc_1 (Location, Count, Value (Value'First)'Access);

      Error := Get_Error;
      if Error /= 0 then
         raise Program_Error with GL.Debug.Image (Error);
      end if;
   end Uniform_Int_Array;

   --------------------
   -- Uniform_Matrix --
   --------------------

   procedure Uniform_Matrix
     (Location  : Uint;
      Count     : Sizei;
      Transpose : GLboolean;
      Matrix    : Array_Of_Float)
   is
      procedure Rho_Uniform_Matrix_F
        (Location                                       : Uint;
         Count                                          : Sizei;
         Transpose                                      : GLboolean;
         A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P : GLfloat);
      pragma Import (C, Rho_Uniform_Matrix_F, "Rho_uniform_matrix_f");
      Error : GLenum;
   begin

      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glUniformMatrix4fv" & Location'Img & Count'Img & Transpose'Img);
         for Row in 1 .. 4 loop
            Ada.Text_IO.Put ("  |");
            for Col in 1 .. 4 loop
               Ada.Float_Text_IO.Put
                 (Float (Matrix ((Col - 1) * 4 + Row)), 3, 3, 0);
            end loop;
            Ada.Text_IO.Put_Line ("|");
         end loop;
      end if;

      Rho_Uniform_Matrix_F
        (Location, Count, Transpose,
         Matrix (1), Matrix (2), Matrix (3), Matrix (4),
         Matrix (5), Matrix (6), Matrix (7), Matrix (8),
         Matrix (9), Matrix (10), Matrix (11), Matrix (12),
         Matrix (13), Matrix (14), Matrix (15), Matrix (16));
      Error := Get_Error;
      if Error /= 0 then
         raise Program_Error with Error'Img;
      end if;
   end Uniform_Matrix;

   -----------------
   -- Use_Program --
   -----------------

   procedure Use_Program
     (Program : Uint)
   is
      type Lib_glUseProgram is access
        procedure
          (Program : Uint);
      pragma Convention (C, Lib_glUseProgram);
      glUseProgram : Lib_glUseProgram;
      pragma Import (C, glUseProgram,
                     "_ptrc_glUseProgram");
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glUseProgram: " & Program'Img);
      end if;
      glUseProgram
        (Program);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Use_Program;

   ------------------------------
   -- Vertex_Attribute_Divisor --
   ------------------------------

   procedure Vertex_Attribute_Divisor
     (Index        : Uint;
      Divisor      : Uint)
   is
      procedure Rho_GlVertexAttribDivisor (Index, Divisor : Uint);
      pragma Import (C, Rho_GlVertexAttribDivisor,
                     "Rho_glVertexAttribDivisor");
      Error : GLenum;
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line ("glVertexAttributeDivisor:"
                               & Index'Img & Divisor'Img);
      end if;

      Rho_GlVertexAttribDivisor
        (Index,
         Divisor);

      Error := Get_Error;
      if Error /= 0 then
         raise Program_Error with Error'Img;
      end if;
   end Vertex_Attribute_Divisor;

   ------------------------------
   -- Vertex_Attribute_Pointer --
   ------------------------------

   procedure Vertex_Attribute_Pointer
     (Index        : Uint;
      Size         : Int;
      Element_Type : GLenum;
      Normalized   : GLboolean;
      Stride       : Sizei;
      Pointer      : System.Storage_Elements.Storage_Offset)
   is
      type Lib_glVertexAttribPointer is access
        procedure
          (Index        : Uint;
           Size         : Int;
           Element_Type : GLenum;
           Normalized   : GLboolean;
           Stride       : Sizei;
           Pointer      : System.Storage_Elements.Storage_Offset);
      pragma Convention (C, Lib_glVertexAttribPointer);
      glVertexAttribPointer : Lib_glVertexAttribPointer;
      pragma Import (C, glVertexAttribPointer,
                     "_ptrc_glVertexAttribPointer");
   begin
      if Debug_Active then
         Ada.Text_IO.Put_Line
           ("glVertexAttribPointer:"
            & Index'Img & Size'Img & " " & GL.Debug.Image (Element_Type)
            & Normalized'Img
            & Stride'Img & Pointer'Img);
      end if;
      glVertexAttribPointer
        (Index,
         Size,
         Element_Type,
         Normalized,
         Stride,
         Pointer);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Vertex_Attribute_Pointer;

   --------------
   -- Viewport --
   --------------

   procedure Viewport
     (X : Int;
      Y : Int;
      W : Sizei;
      H : Sizei)
   is
      type Lib_glViewport is access
        procedure
          (X : Int;
           Y : Int;
           W : Sizei;
           H : Sizei);
      pragma Convention (C, Lib_glViewport);
      glViewport : Lib_glViewport;
      pragma Import (C, glViewport,
                     "_ptrc_glViewport");
   begin
      glViewport
        (X,
         Y,
         W,
         H);
      if Get_Error /= 0 then
         raise Program_Error;
      end if;
   end Viewport;

end GL;
