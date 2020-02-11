with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

--  with Ada.Unchecked_Conversion;

with Rho.Rendering.GL_Renderer.GL_Window;

with GLUT;

with GL;
with GL_Constants;                  use GL_Constants;
with GL_Types;                      use GL_Types;

with Interfaces.C;

with System;
--  with System.Address_To_Access_Conversions;

with Glib;
with Cairo.Surface;
with Cairo.Image_Surface;
with Interfaces.C.Strings;

package body Rho.Rendering.GL_Renderer is

   type Aliased_Ubyte_Array is
     array (GL_Types.Int range <>) of aliased Ubyte;

   type Aliased_Ubyte_Array_Access is
     access all Aliased_Ubyte_Array;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Aliased_Ubyte_Array, Aliased_Ubyte_Array_Access);

   type Rho_GL_Renderer_Record is
     new Rho_Renderer_Record with
      record
         Top_Window     : GL_Window.Rho_GL_Window;
         Active_Texture : Rho.Texture.Texture_Id;
      end record;

   type Rho_GL_Renderer is access all Rho_GL_Renderer_Record'Class;

   overriding function Current_Render_Target
     (Renderer : Rho_GL_Renderer_Record)
      return Rho.Render_Target.Rho_Render_Target
   is (Rho.Render_Target.Rho_Render_Target (Renderer.Top_Window));

   overriding procedure Render_Loop
     (Renderer : in out Rho_GL_Renderer_Record);

   overriding procedure EXIT_Render_Loop
     (Renderer : in out Rho_GL_Renderer_Record);

   overriding function Create_Top_Level_Window
     (Renderer : in out Rho_GL_Renderer_Record)
      return Rho.Render_Window.Rho_Render_Window;

   overriding procedure Render_One_Frame
     (Renderer : in out Rho_GL_Renderer_Record);

   overriding function Create_Surface
     (Renderer      : in out Rho_GL_Renderer_Record;
      Width, Height : Rho_Float)
      return Cairo.Cairo_Surface;

   overriding function Load_Texture
     (Renderer   : in out Rho_GL_Renderer_Record;
      S_Wrap     : Rho.Texture.Texture_Address_Mode;
      T_Wrap     : Rho.Texture.Texture_Address_Mode;
      Mag_Filter : Rho.Texture.Texture_Filter_Type)
      return Rho.Texture.Texture_Id;

   overriding procedure Load_Texture_Data
     (Renderer     : in out Rho_GL_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Surface : Cairo.Cairo_Surface;
      Region       : Rho.Rectangle.Rho_Rectangle);

   overriding procedure Load_Texture_Data
     (Renderer     : in out Rho_GL_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Data    : Rho.Color.Rho_Color_2D_Array);

   --     overriding function Load_Vertex_Array
   --       (Renderer : in out Rho_GL_Renderer_Record;
   --        Vertex_Array : Rho.Vertex_Array.Rho_Vertex_Array_Record'Class)
   --        return Rho.Vertex_Array.Rho_Vertex_Array_Id;

   overriding function Load_Buffer
     (Renderer : in out Rho_GL_Renderer_Record;
      Buffer   : Rho.Float_Buffer.Rho_Float_Buffer_Record'Class)
      return Rho.Float_Buffer.Rho_Float_Buffer_Id;

--     function Load_Program
--       (Vertex_Shader_Path   : String;
--        Fragment_Shader_Path : String)
--        return Rho.Shader.Rho_Program;

   -------------------------
   -- Create_GL_Renderer --
   -------------------------

   function Create_GL_Renderer
     return Rho_Renderer
   is
      Result : constant Rho_GL_Renderer :=
                 new Rho_GL_Renderer_Record;
      use type Interfaces.C.unsigned;
   begin
      Rho.Mouse.Set_Current_Mouse (GL_Mouse'Access);
      GLUT.Init;
      GLUT.Init_Display_Mode
        (Mode => GLUT.DOUBLE or GLUT.RGB or GLUT.DEPTH);

      return Rho_Renderer (Result);
   end Create_GL_Renderer;

   --------------------
   -- Create_Surface --
   --------------------

   overriding function Create_Surface
     (Renderer      : in out Rho_GL_Renderer_Record;
      Width, Height : Rho_Float)
      return Cairo.Cairo_Surface
   is
      pragma Unreferenced (Renderer);
   begin
      return Cairo.Image_Surface.Create
        (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
         Width  => Glib.Gint (Width),
         Height => Glib.Gint (Height));
   end Create_Surface;

   -----------------------------
   -- Create_Top_Level_Window --
   -----------------------------

   overriding function Create_Top_Level_Window
     (Renderer : in out Rho_GL_Renderer_Record)
      return Rho.Render_Window.Rho_Render_Window
   is
      use type Interfaces.C.int;
      function Load_Functions return Interfaces.C.int;
      pragma Import (C, Load_Functions, "ogl_LoadFunctions");

   begin

      Renderer.Top_Window :=
        GL_Window.Create_Top_Level_Window;

      if Load_Functions = 0 then
         raise Render_Error with "cannot load OpenGL";
      end if;

      Renderer.Top_Window.Set_Back_Face_Removal (True);
      Renderer.Top_Window.Set_Wireframe (False);
      Renderer.Top_Window.Set_Depth_Test (True);

      Ada.Text_IO.Put_Line
        ("Maas-GL: "
         & Interfaces.C.Strings.Value (GL.Get_String (GL_VENDOR))
         & ": "
         & Interfaces.C.Strings.Value (GL.Get_String (GL_VERSION)));

      return Rho.Render_Window.Rho_Render_Window (Renderer.Top_Window);
   end Create_Top_Level_Window;

   ----------------------
   -- EXIT_Render_Loop --
   ----------------------

   overriding procedure EXIT_Render_Loop
     (Renderer : in out Rho_GL_Renderer_Record)
   is
      pragma Unreferenced (Renderer);
   begin
      GLUT.Leave_Main_Loop;
   end EXIT_Render_Loop;

   ------------------------
   -- GL_Error_To_String --
   ------------------------

   function GL_Error_To_String
     (Error : GLenum)
      return String
   is
   begin
      case Error is
         when 0 =>
            return "No error";
         when GL_INVALID_ENUM =>
            return "Invalid enum";
         when GL_INVALID_VALUE =>
            return "Invalid value";
         when GL_INVALID_OPERATION =>
            return "Invalid operation";
         when GL_INVALID_FRAMEBUFFER_OPERATION =>
            return "Invalid framebuffer operation";
         when GL_OUT_OF_MEMORY =>
            return "Out of memory";
         when others =>
            return "Unknown error" & GLenum'Image (Error);
      end case;
   end GL_Error_To_String;

   -----------------
   -- Load_Buffer --
   -----------------

   overriding function Load_Buffer
     (Renderer : in out Rho_GL_Renderer_Record;
      Buffer   : Rho.Float_Buffer.Rho_Float_Buffer_Record'Class)
      return Rho.Float_Buffer.Rho_Float_Buffer_Id
   is
      pragma Unreferenced (Renderer);
      type Array_Of_Float_Access is
        access GL_Types.Array_Of_Float;
      procedure Free is
        new Ada.Unchecked_Deallocation
          (GL_Types.Array_Of_Float, Array_Of_Float_Access);

      GL_Values  : Array_Of_Float_Access :=
                     new GL_Types.Array_Of_Float
                       (1 .. Buffer.Count);

      Buffer_Id  : aliased Uint;
   begin
      for I in GL_Values'Range loop
         GL_Values (I) := GLfloat (Buffer.Value (I));
      end loop;

      GL.Gen_Buffers (1, Buffer_Id'Access);
      GL.Bind_Buffer (GL_ARRAY_BUFFER, Buffer_Id);

      GL.Buffer_Data (GL_ARRAY_BUFFER,
                      GL_Values.all'Size / System.Storage_Unit,
                      GL_Values.all'Address, GL_STATIC_DRAW);

      Free (GL_Values);

--        GL.Vertex_Attribute_Pointer
--          (Index        => 0,
--           Size         => 3,
--           Element_Type => GL_FLOAT,
--           Normalized   => GL_FALSE,
--           Stride       => 0,
--           Pointer      => Buffer.To_Offset (Vertex_Start));
--        GL.Enable_Vertex_Attribute_Array (0);
--
--        if Texture_Start > 0 then
--           GL.Vertex_Attribute_Pointer
--             (Index        => 1,
--              Size         => 2,
--              Element_Type => GL_FLOAT,
--              Normalized   => GL_FALSE,
--              Stride       => 0,
--              Pointer      => Buffer.To_Offset (Texture_Start));
--           GL.Enable_Vertex_Attribute_Array (1);
--        end if;

      return Rho.Float_Buffer.Rho_Float_Buffer_Id (Buffer_Id);
   end Load_Buffer;

   ------------------
   -- Load_Program --
   ------------------

--     function Load_Program
--       (Vertex_Shader_Path   : String;
--        Fragment_Shader_Path : String)
--        return Rho.Shader.Rho_Program
--     is
--
--        Vertex_Shader   : constant Rho.Shader.Rho_Shader :=
--                            Rho.Shader.Load (Rho.Paths.Config_Path
--                                            & "/shaders/"
--                                            & Vertex_Shader_Path,
--                                            Rho.Shader.Vertex);
--        Fragment_Shader : constant Rho.Shader.Rho_Shader :=
--                            Rho.Shader.Load (Rho.Paths.Config_Path
--                                            & "/shaders/"
--                                            & Fragment_Shader_Path,
--                                            Rho.Shader.Fragment);
--        Result          : constant Rho.Shader.Rho_Program :=
--                            Rho.Shader.Create;
--     begin
--        Result.Add (Vertex_Shader);
--        Result.Add (Fragment_Shader);
--
--        Result.Compile;
--
--        return Result;
--
--     end Load_Program;

   ------------------
   -- Load_Texture --
   ------------------

   overriding function Load_Texture
     (Renderer     : in out Rho_GL_Renderer_Record;
      S_Wrap       : Rho.Texture.Texture_Address_Mode;
      T_Wrap       : Rho.Texture.Texture_Address_Mode;
      Mag_Filter   : Rho.Texture.Texture_Filter_Type)
      return Rho.Texture.Texture_Id
   is
      use Rho.Texture;

      Id           : array (1 .. 1) of aliased Uint;
      To_GL_Wrap   : constant array (Texture_Address_Mode) of GLenum :=
                       (Border => GL_REPEAT, Clamp => GL_CLAMP,
                        Mirror => GL_CLAMP, Wrap => GL_REPEAT);
      To_GL_Filter : constant array (Texture_Filter_Type) of GLenum :=
                       (Nearest => GL_NEAREST, Linear => GL_LINEAR);

      GL_S_Wrap     : constant GLenum := To_GL_Wrap (S_Wrap);
      GL_T_Wrap     : constant GLenum := To_GL_Wrap (T_Wrap);
      GL_Mag_Filter : constant GLenum :=
                        To_GL_Filter (Mag_Filter);

      --        function Tex_Arg is
      --          new Ada.Unchecked_Conversion (GL.Texture_Wrap_Mode, GL.Int);
      --
      --        function Tex_Arg is
      --          new Ada.Unchecked_Conversion (GL.Texture_Mag_Filter, GL.Int);
      --
      --        function Tex_Arg is
      --          new Ada.Unchecked_Conversion (GL.Texture_Min_Filter, GL.Int);
      --
      --        Width : constant Gint :=
      --                  Cairo.Image_Surface.Get_Width (From_Surface);
      --        Height : constant Gint :=
      --                   Cairo.Image_Surface.Get_Height (From_Surface);

   begin
      GL.Gen_Textures (1, Id (Id'First)'Access);
      GL.Bind_Texture (GL_TEXTURE_2D, Id (1));

      GL.Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_S_Wrap);
      GL.Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_T_Wrap);
      GL.Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_Mag_Filter);
      GL.Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_Mag_Filter);

      Renderer.Active_Texture := Rho.Texture.Texture_Id (Id (1));
      return Renderer.Active_Texture;
   end Load_Texture;

   -----------------------
   -- Load_Texture_Data --
   -----------------------

   overriding procedure Load_Texture_Data
     (Renderer     : in out Rho_GL_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Data    : Rho.Color.Rho_Color_2D_Array)
   is
      Width : constant Int := Int (From_Data'Length (1));
      Height : constant Int := Int (From_Data'Length (2));

      Dest_Data   : Aliased_Ubyte_Array_Access;
   begin
      if Height = 0 or else Width = 0 then
         return;
      end if;

      Dest_Data :=
        new Aliased_Ubyte_Array (0 .. 4 * Width * Height - 1);

      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            declare
               Src_X        : constant Positive := Positive (X + 1);
               Src_Y        : constant Positive := Positive (Y + 1);
               Dest_Index   : constant Int :=
                                4 * X + (Height - Y - 1) * Width * 4;

               function To_Ubyte (X : Unit_Float) return Ubyte
               is (Ubyte (X * 255.0));

            begin
               Dest_Data (Dest_Index) :=
                 To_Ubyte (From_Data (Src_X, Src_Y).Red);
               Dest_Data (Dest_Index + 1) :=
                 To_Ubyte (From_Data (Src_X, Src_Y).Green);
               Dest_Data (Dest_Index + 2) :=
                 To_Ubyte (From_Data (Src_X, Src_Y).Blue);
               Dest_Data (Dest_Index + 3) :=
                 To_Ubyte (From_Data (Src_X, Src_Y).Alpha);
            end;
         end loop;
      end loop;

      declare
         use type Rho.Texture.Texture_Id;
      begin
         if Renderer.Active_Texture /= Texture_Id then
            GL.Bind_Texture (GL_TEXTURE_2D, Uint (Texture_Id));
            Renderer.Active_Texture := Texture_Id;
         end if;
      end;

      GL.Tex_Image_2D
        (Target          => GL_TEXTURE_2D,
         Level           => 0,
         Internal_Format => GL_RGBA,
         Width           => Sizei (Width),
         Height          => Sizei (Height),
         Border          => 0,
         Format          => GL_RGBA,
         Ptype           => GL_UNSIGNED_BYTE,
         Pixels          => Dest_Data (0)'Address);

      Free (Dest_Data);

   end Load_Texture_Data;

   -----------------------
   -- Load_Texture_Data --
   -----------------------

   overriding procedure Load_Texture_Data
     (Renderer     : in out Rho_GL_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Surface : Cairo.Cairo_Surface;
      Region       : Rho.Rectangle.Rho_Rectangle)
   is
      use Glib;
      Data       : constant System.Address :=
                     Cairo.Image_Surface.Get_Data_Generic (From_Surface);
      Stride     : constant Glib.Gint :=
                     Cairo.Image_Surface.Get_Stride (From_Surface);
      Src_Width  : constant Gint :=
                     Cairo.Image_Surface.Get_Width (From_Surface);
      Src_Height : constant Gint :=
                     Cairo.Image_Surface.Get_Height (From_Surface);
      Start_X    : constant Gint := Gint (Region.X);
      Start_Y    : constant Gint := Gint (Region.Y);
      Width      : constant Natural := Natural (Region.Width);
      Height     : constant Natural := Natural (Region.Height);

      type Source_Data_Array is array (Gint range <>) of aliased Ubyte;
      Source_Data : Source_Data_Array (0 .. Stride * Src_Height - 1);
      for Source_Data'Address use Data;
      Dest_Data : Rho.Color.Rho_Color_2D_Array_Access;

      procedure Set_Dest (Source_Index : Gint;
                          X, Y         : Positive);

      --------------
      -- Set_Dest --
      --------------

      procedure Set_Dest (Source_Index : Gint;
                          X, Y         : Positive)
      is
         Red   : constant Ubyte := Source_Data (Source_Index + 2);
         Green : constant Ubyte := Source_Data (Source_Index + 1);
         Blue  : constant Ubyte := Source_Data (Source_Index + 0);
         Alpha : constant Ubyte := Source_Data (Source_Index + 3);
      begin
         Dest_Data (X, Y) :=
           (Red   => Rho_Float (Red) / 255.0,
            Green => Rho_Float (Green) / 255.0,
            Blue  => Rho_Float (Blue) / 255.0,
            Alpha => Rho_Float (Alpha) / 255.0);
      end Set_Dest;

   begin
      Cairo.Surface.Flush (From_Surface);

      if Height = 0 or else Width = 0 then
         return;
      end if;

      Dest_Data := new Rho.Color.Rho_Color_2D_Array (1 .. Width, 1 .. Height);

      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            declare
               Src_X        : constant Gint := Gint (X) + Start_X;
               Src_Y        : constant Gint := Gint (Y) + Start_Y;
               Source_Index : constant Gint :=
                                (if Src_X < Src_Width
                                 and then Src_Y < Src_Height
                                 then 4 * Src_X + Src_Y * Stride
                                 else 0);
            begin
               if Src_X < Src_Width and then Src_Y < Src_Height then
                  Set_Dest (Source_Index, X + 1, Y + 1);
               else
                  Dest_Data (X + 1, Y + 1) := (0.0, 0.0, 0.0, 0.0);
               end if;
            end;
         end loop;
      end loop;

      Renderer.Load_Texture_Data
        (Texture_Id, Dest_Data.all);

      Rho.Color.Free (Dest_Data);

   end Load_Texture_Data;

   -----------------
   -- Render_Loop --
   -----------------

   overriding procedure Render_Loop
     (Renderer : in out Rho_GL_Renderer_Record)
   is
      pragma Unreferenced (Renderer);
   begin
      GLUT.Main_Loop;
   end Render_Loop;

   ----------------------
   -- Render_One_Frame --
   ----------------------

   overriding procedure Render_One_Frame
     (Renderer : in out Rho_GL_Renderer_Record)
   is
   begin
      Renderer.Top_Window.Render;
   end Render_One_Frame;

end Rho.Rendering.GL_Renderer;
