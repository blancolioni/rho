with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with System;
with Interfaces.C;
with Interfaces.C.Strings;

with Glib;

with Cairo.Png;
with Cairo.Surface;
with Cairo.Image_Surface;

with GLUT;

with GL;
with GL_Constants;
with GL_Types;

with WL.String_Maps;

with Tau.Environment;

with Rho.Buffers;
with Rho.Color;
with Rho.Matrices;
with Rho.Rectangles;
with Rho.Shaders.Slices.Attributes;
with Rho.Shaders.Slices.Main;
with Rho.Shaders.Slices.Preamble;
with Rho.Shaders.Slices.Uniforms;
with Rho.Shaders.Programs;
with Rho.Shaders.Stages;
with Rho.Shaders.Variables;
with Rho.Signals;
with Rho.Textures;
with Rho.Values;

with Rho.Real_Arrays;

with Rho.Handles.OpenGL.Maps;

with Rho.Logging;

package body Rho.Handles.OpenGL is

   type Aliased_Ubyte_Array is
     array (GL_Types.Int range <>) of aliased GL_Types.Ubyte;

   type Aliased_Ubyte_Array_Access is
     access all Aliased_Ubyte_Array;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Aliased_Ubyte_Array, Aliased_Ubyte_Array_Access);

   function Read_File (Path : String) return String with Unreferenced;

   package Uniform_Value_Maps is
     new WL.String_Maps (Rho.Values.Rho_Value, Rho.Values."=");

   package Shader_Variable_Maps is
     new WL.String_Maps (Rho.Shaders.Variables.Variable_Type,
                         Rho.Shaders.Variables."=");

   type OpenGL_Asset_Container is
     new Rho.Assets.Root_Asset_Container_Type with
      record
         null;
      end record;

   overriding function Generator_Name
     (Container : OpenGL_Asset_Container)
      return String
   is ("glsl");

   overriding function Create_Texture_From_Image
     (Container : in out OpenGL_Asset_Container;
      Path      : String)
      return Rho.Textures.Texture_Type;

   package Shader_Slices is
     new Ada.Containers.Vectors
       (Positive, Rho.Shaders.Slices.Slice_Type,
        Rho.Shaders.Slices."=");

   type OpenGL_Render_Target is
     new Rho.Signals.Signal_Dispatcher
     and Rho.Render.Render_Target with
      record
         Assets            : Rho.Assets.Asset_Container_Type;
         Active_Program    : Rho.Shaders.Programs.Program_Type;
         Active_Projection : Rho.Matrices.Matrix_4;
         Active_Model_View : Rho.Matrices.Matrix_4;
         Active_Fragments  : Shader_Slices.Vector;
         Active_Uniforms   : Uniform_Value_Maps.Map;
         Active_Variables  : Shader_Variable_Maps.Map;
         Active_Texture_Id : Natural := 0;
         Current_Count     : Natural := 0;
         Id_Map            : Rho.Handles.OpenGL.Maps.Id_Map;
      end record;

   type OpenGL_Render_Target_Access is
     access all OpenGL_Render_Target'Class;

   --  overriding function Assets
   --    (Target : OpenGL_Render_Target)
   --     return Rho.Assets.Asset_Container_Type
   --  is (Target.Assets);

   overriding function Active_Shader_Slices
     (Target : OpenGL_Render_Target)
      return Rho.Shaders.Slices.Slice_Array;

   overriding procedure Add_Shader_Fragment
     (Target   : in out OpenGL_Render_Target;
      Slice : Rho.Shaders.Slices.Slice_Type);

   overriding procedure Add_Uniform
     (Target  : in out OpenGL_Render_Target;
      Name    : String;
      Value   : Rho.Values.Rho_Value);

   overriding procedure Activate_Shader
     (Target : in out OpenGL_Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type);

   overriding procedure Bind_Shader
     (Target : in out OpenGL_Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type);

   overriding procedure Compile_Shader
     (Target : in out OpenGL_Render_Target;
      Shader    : Rho.Shaders.Stages.Shader_Type);

   overriding function Create_Program
     (Target : in out OpenGL_Render_Target;
      Name      : String;
      Shaders   : Rho.Shaders.Stages.Shader_Array)
      return Rho.Shaders.Programs.Program_Type;

   overriding function Current_Shader
     (Target : OpenGL_Render_Target)
      return Rho.Shaders.Programs.Program_Type
   is (Target.Active_Program);

   overriding procedure Set_Projection_Matrix
     (Target : in out OpenGL_Render_Target;
      Matrix : Rho.Matrices.Matrix_4);

   overriding procedure Set_Model_View_Matrix
     (Target : in out OpenGL_Render_Target;
      Matrix : Rho.Matrices.Matrix_4);

   overriding procedure Render_Current_Buffers
     (Target : in out OpenGL_Render_Target);

   overriding procedure Load_Buffer
     (Target : in out OpenGL_Render_Target;
      Buffer  : Rho.Buffers.Buffer_Type);

   overriding procedure Activate_Buffer
     (Target   : in out OpenGL_Render_Target;
      Buffer   : Rho.Buffers.Buffer_Type;
      Argument : not null access
        Rho.Shaders.Variables.Root_Variable_Type'Class);

   procedure Bind_Uniform
     (Target   : in out OpenGL_Render_Target'Class;
      Variable : Rho.Shaders.Variables.Variable_Type;
      Value    : Rho.Values.Rho_Value);

   procedure Load_Texture_Data
     (Target     : not null access OpenGL_Render_Target'Class;
      Texture_Id : Natural;
      Width      : Positive;
      Height     : Positive;
      Data       : Aliased_Ubyte_Array_Access);

   Local_Render_Target : aliased OpenGL_Render_Target :=
     OpenGL_Render_Target'
       (Rho.Signals.Signal_Dispatcher with
        Assets            => new OpenGL_Asset_Container,
        others            => <>);

   type OpenGL_Texture_Type is
     new Rho.Textures.Root_Texture_Type with
      record
         Id           : Natural;
         Surface      : Cairo.Cairo_Surface;
      end record;

   type Texture_Access is access all OpenGL_Texture_Type'Class;

   overriding procedure Load
     (Texture : in out OpenGL_Texture_Type;
      Target   : not null access Rho.Render.Render_Target'Class);

   overriding procedure Unload
     (Texture : in out OpenGL_Texture_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is null;

   overriding procedure Activate
     (Texture : OpenGL_Texture_Type;
      Target  : not null access Rho.Render.Render_Target'Class);

   type OpenGL_Handle is
     new Root_Handle_Type with
      record
         null;
      end record;

   overriding function Create_Window
     (Handle : in out OpenGL_Handle;
      X      : Real;
      Y      : Real;
      Width  : Non_Negative_Real;
      Height : Non_Negative_Real;
      Full   : Boolean)
      return Rho.Windows.Window_Type;

   overriding procedure Main_Loop
     (Handle : in out OpenGL_Handle);

   overriding function Current_Renderer
     (Handle : OpenGL_Handle)
      return not null access Rho.Render.Render_Target'Class
   is (Local_Render_Target'Access);

   Local_Handle : aliased OpenGL_Handle;

   type OpenGL_Window is
     new Rho.Windows.Root_Window_Type with
      record
         Id : Integer;
      end record;

   overriding procedure Before_Render
     (Window : in out OpenGL_Window);

   overriding procedure After_Render
     (Window : in out OpenGL_Window);

   overriding function Render_Target
     (Window : OpenGL_Window)
      return access Rho.Render.Render_Target'Class
   is (Local_Render_Target'Access);

   procedure Display_Handler;

   procedure Reshape_Handler
     (Width, Height : Integer);

   procedure Idle_Handler;

   function To_GL_Float_Array
     (Matrix : Rho.Matrices.Matrix_4)
      return GL_Types.Float_Matrix_4x4;

   function To_Color_Array
     (From_Surface : Cairo.Cairo_Surface)
      return Aliased_Ubyte_Array_Access;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Texture : OpenGL_Texture_Type;
      Target  : not null access Rho.Render.Render_Target'Class)
   is
   begin
      GL.Bind_Texture (GL_Constants.GL_TEXTURE_2D, GL_Types.Uint (Texture.Id));
   end Activate;

   ---------------------
   -- Activate_Buffer --
   ---------------------

   overriding procedure Activate_Buffer
     (Target   : in out OpenGL_Render_Target;
      Buffer   : Rho.Buffers.Buffer_Type;
      Argument : not null access
        Rho.Shaders.Variables.Root_Variable_Type'Class)
   is
      Buffer_Id : constant GL_Types.Uint := Target.Id_Map.Buffer_Id (Buffer);
   begin
      case Buffer.Contents is
         when Rho.Buffers.Integer_Data =>
            GL.Bind_Buffer (GL_Constants.GL_ELEMENT_ARRAY_BUFFER, Buffer_Id);
            Target.Current_Count := Buffer.Element_Count;
         when others =>
            GL.Bind_Buffer (GL_Constants.GL_ARRAY_BUFFER, Buffer_Id);
            GL.Vertex_Attribute_Pointer
              (Index        => Target.Id_Map.Variable_Id (Argument),
               Size         => GL_Types.Int (Argument.Element_Count),
               Element_Type => GL_Constants.GL_FLOAT,
               Normalized   => GL_Constants.GL_FALSE,
               Stride       => 0,
               Pointer      => 0);
            GL.Enable_Vertex_Attribute_Array
              (Target.Id_Map.Variable_Id (Argument));
      end case;
   end Activate_Buffer;

   ---------------------
   -- Activate_Shader --
   ---------------------

   overriding procedure Activate_Shader
     (Target : in out OpenGL_Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type)
   is
      use type Rho.Shaders.Programs.Program_Type;
   begin
      if Target.Active_Program /= Shader then
         GL.Use_Program
           (Target.Id_Map.Program_Id (Shader));
         Target.Active_Program := Shader;
      end if;
   end Activate_Shader;

   -----------------------------
   -- Active_Shader_Slices --
   -----------------------------

   overriding function Active_Shader_Slices
     (Target : OpenGL_Render_Target)
      return Rho.Shaders.Slices.Slice_Array
   is
   begin
      return Arr : Rho.Shaders.Slices.Slice_Array
        (1 .. Target.Active_Fragments.Last_Index)
      do
         for I in Arr'Range loop
            Arr (I) := Target.Active_Fragments (I);
         end loop;
      end return;
   end Active_Shader_Slices;

   -------------------------
   -- Add_Shader_Fragment --
   -------------------------

   overriding procedure Add_Shader_Fragment
     (Target   : in out OpenGL_Render_Target;
      Slice : Rho.Shaders.Slices.Slice_Type)
   is
   begin
      Target.Active_Fragments.Append (Slice);
   end Add_Shader_Fragment;

   -----------------
   -- Add_Uniform --
   -----------------

   overriding procedure Add_Uniform
     (Target  : in out OpenGL_Render_Target;
      Name    : String;
      Value   : Rho.Values.Rho_Value)
   is
   begin
      Rho.Logging.Log
        ("new active uniform: " & Name);
      Target.Active_Uniforms.Insert (Name, Value);
   end Add_Uniform;

   ------------------
   -- After_Render --
   ------------------

   overriding procedure After_Render
     (Window : in out OpenGL_Window)
   is
      pragma Unreferenced (Window);
   begin
      GLUT.Swap_Buffers;
   end After_Render;

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Window : in out OpenGL_Window)
   is
      use GL_Types, GL_Constants;
      W : OpenGL_Window'Class renames OpenGL_Window'Class (Window);
      Clear : constant Rho.Color.Color_Type := W.Clear_Color;
   begin
      GL.Clear_Color (GLfloat (Clear.R), GLfloat (Clear.G),
                      GLfloat (Clear.B), GLfloat (Clear.A));
      GL.Clear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      if Window.Wireframe then
         GL.Polygon_Mode (GL_FRONT_AND_BACK, GL_LINE);
      else
         GL.Polygon_Mode (GL_FRONT_AND_BACK, GL_FILL);
      end if;

   end Before_Render;

   -----------------
   -- Bind_Shader --
   -----------------

   overriding procedure Bind_Shader
     (Target : in out OpenGL_Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type)
   is
   begin
      Target.Activate_Shader (Shader);
      GL.Uniform_Matrix
        (Location  => Target.Id_Map.Variable_Id (Shader.Projection_Uniform),
         Count     => 1,
         Transpose => GL_Constants.GL_FALSE,
         Matrix    => To_GL_Float_Array (Target.Active_Projection));
      GL.Uniform_Matrix
        (Location  => Target.Id_Map.Variable_Id (Shader.Model_View_Uniform),
         Count     => 1,
         Transpose => GL_Constants.GL_FALSE,
         Matrix    => To_GL_Float_Array (Target.Active_Model_View));

      for Position in Target.Active_Uniforms.Iterate loop
         declare
            Name     : constant String := Uniform_Value_Maps.Key (Position);
            Value    : constant Rho.Values.Rho_Value :=
                         Uniform_Value_Maps.Element (Position);
            Variable : constant Rho.Shaders.Variables.Variable_Type :=
                         Shader.Get_Variable (Name);
         begin
            Target.Bind_Uniform (Variable, Value);
         end;
      end loop;
   end Bind_Shader;

   ------------------
   -- Bind_Uniform --
   ------------------

   procedure Bind_Uniform
     (Target   : in out OpenGL_Render_Target'Class;
      Variable : Rho.Shaders.Variables.Variable_Type;
      Value    : Rho.Values.Rho_Value)
   is
      use all type Rho.Values.Value_Type;
      Id : constant GL_Types.Uint :=
             Target.Id_Map.Variable_Id (Variable);
   begin
      case Value.Of_Type is
         when Real_Value =>
            GL.Uniform (Id, GL_Types.GLfloat (Rho.Values.To_Real (Value)));
         when Vector_2_Value =>
            declare
               Vec_2 : constant Rho.Matrices.Vector_2 :=
                         Rho.Values.To_Vector_2 (Value);
            begin
               GL.Uniform
                 (Id,
                  (GL_Types.GLfloat (Rho.Matrices.X (Vec_2)),
                   GL_Types.GLfloat (Rho.Matrices.Y (Vec_2))));
            end;

         when Vector_3_Value =>
            declare
               Vec_3 : constant Rho.Matrices.Vector_3 :=
                         Rho.Values.To_Vector_3 (Value);
            begin
               GL.Uniform
                 (Id,
                  (GL_Types.GLfloat (Rho.Matrices.X (Vec_3)),
                   GL_Types.GLfloat (Rho.Matrices.Y (Vec_3)),
                   GL_Types.GLfloat (Rho.Matrices.Z (Vec_3))));
            end;
         when others =>
            raise Constraint_Error with
            Value.Of_Type'Image & " uniform not supported";
      end case;
   end Bind_Uniform;

   --------------------
   -- Compile_Shader --
   --------------------

   overriding procedure Compile_Shader
     (Target : in out OpenGL_Render_Target;
      Shader    : Rho.Shaders.Stages.Shader_Type)
   is
      GL_Stage : constant GL_Types.GLenum :=
                   (case Shader.Stage is
                       when Vertex_Shader   =>
                         GL_Constants.GL_VERTEX_SHADER,
                       when Fragment_Shader =>
                         GL_Constants.GL_FRAGMENT_SHADER);
      pragma Assert (Shader.Has_Source);

      Source   : constant String := Shader.Shader_Source;
      Id       : constant GL_Types.Uint :=
                   GL.Create_Shader (GL_Stage);
   begin
      GL.Shader_Source
        (Shader => Id,
         Count  => 1,
         Source => Source);
      GL.Compile_Shader (Id);

      declare
         use GL_Constants, GL_Types;
         Status     : constant Int := GL.Get_Compile_Status (Id);
         Log_Length : aliased Int;
      begin
         GL.Get_Shader (Id, GL_INFO_LOG_LENGTH, Log_Length'Access);
         if Log_Length > 0 then
            declare
               Log : constant Interfaces.C.Strings.char_array_access :=
                       new Interfaces.C.char_array
                         (1 .. Interfaces.C.size_t (Log_Length));
            begin
               GL.Get_Shader_Info_Log
                 (Id, Sizei (Log_Length), null,
                  Interfaces.C.Strings.To_Chars_Ptr (Log));
               Ada.Text_IO.Put_Line (Interfaces.C.To_Ada (Log.all));
            end;
         end if;

         if Status = 0 then
            raise Constraint_Error with
            Shader.Name & " failed to compile";
         end if;
      end;

      Local_Render_Target.Id_Map.Define_Shader (Shader, Id);
      Shader.Set_Loaded;

   end Compile_Shader;

   --------------------
   -- Create_Program --
   --------------------

   overriding function Create_Program
     (Target : in out OpenGL_Render_Target;
      Name      : String;
      Shaders   : Rho.Shaders.Stages.Shader_Array)
      return Rho.Shaders.Programs.Program_Type
   is
      Id : constant GL_Types.Uint :=
        GL.Create_Program;
   begin
      for Shader of Shaders loop
         GL.Attach_Shader
           (Id, Local_Render_Target.Id_Map.Shader_Id (Shader));
      end loop;
      GL.Link_Program (Id);

      return Program : constant Rho.Shaders.Programs.Program_Type :=
        Rho.Shaders.Programs.Create_Program (Name)
      do
         declare
            procedure Bind_Variable
              (Variable : not null access
                 Rho.Shaders.Variables.Root_Variable_Type'Class);

            -------------------
            -- Bind_Variable --
            -------------------

            procedure Bind_Variable
              (Variable : not null access
                 Rho.Shaders.Variables.Root_Variable_Type'Class)
            is
               use GL_Types;
               use all type Rho.Shaders.Binding_Type;
               Variable_Id : Int := 0;
            begin
               case Variable.Binding is
                  when Attribute_Binding =>
                     Variable_Id :=
                       GL.Get_Attribute_Location (Id, Variable.Name);
                  when Uniform_Binding =>
                     Variable_Id :=
                       GL.Get_Uniform_Location (Id, Variable.Name);
               end case;
               if Variable_Id >= 0 then
                  Local_Render_Target.Id_Map.Define_Variable
                    (Variable, Variable_Id);
               end if;
            end Bind_Variable;

         begin
            Program.Iterate_Variables (Bind_Variable'Access);

            for Position in Target.Active_Uniforms.Iterate loop
               declare
                  Name : constant String := Uniform_Value_Maps.Key (Position);
                  Variable : constant Rho.Shaders.Variables.Variable_Type :=
                               Rho.Shaders.Variables.New_Uniform_Binding
                                 (Program       => Program,
                                  Name          => Name);
               begin
                  Program.Add_Variable (Variable);
                  Bind_Variable (Variable);
               end;
            end loop;
         end;

         Local_Render_Target.Id_Map.Define_Program (Program, Id);
      end return;
   end Create_Program;

   -------------------------------
   -- Create_Texture_From_Image --
   -------------------------------

   overriding function Create_Texture_From_Image
     (Container : in out OpenGL_Asset_Container;
      Path      : String)
      return Rho.Textures.Texture_Type
   is
      Surface : Cairo.Cairo_Surface;
   begin
      Surface :=
        Cairo.Png.Create_From_Png
          (Path & ".png");
      case Cairo.Surface.Status (Surface) is
         when Cairo.Cairo_Status_Success =>
            declare
               Width : constant Glib.Gint :=
                         Cairo.Image_Surface.Get_Width (Surface);
               Height : constant Glib.Gint :=
                         Cairo.Image_Surface.Get_Height (Surface);
               Texture : constant Texture_Access := new OpenGL_Texture_Type;
            begin
               Texture.Initialize
                 (Identifier => Path,
                  Order      => 2,
                  Width      => Natural (Width),
                  Height     => Natural (Height));
               Texture.Surface := Surface;
               return Rho.Textures.Texture_Type (Texture);
            end;

         when Cairo.Cairo_Status_File_Not_Found =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "texture not found: " & Path);
            return null;

         when others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Texture: " & Path);
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "unknown error "
               & Cairo.Cairo_Status'Image
                 (Cairo.Surface.Status (Surface)));
            return null;
      end case;

   end Create_Texture_From_Image;

   -------------------
   -- Create_Window --
   -------------------

   overriding function Create_Window
     (Handle : in out OpenGL_Handle;
      X      : Real;
      Y      : Real;
      Width  : Non_Negative_Real;
      Height : Non_Negative_Real;
      Full   : Boolean)
      return Rho.Windows.Window_Type
   is
      pragma Unreferenced (Full);
      use type Interfaces.C.int;
      Window_Id : Integer;
      function Load_Functions return Interfaces.C.int;
      pragma Import (C, Load_Functions, "ogl_LoadFunctions");
   begin

      GLUT.Init_Window_Size (Integer (Width), Integer (Height));
      GLUT.Init_Window_Position (Integer (X), Integer (Y));

      Window_Id := GLUT.Create_Window ("Rho GL");

      GLUT.Display_Function (Display_Handler'Access);
      GLUT.Reshape_Function (Reshape_Handler'Access);
--        GLUT.Mouse_Function (Mouse_Button_Handler'Access);
--        GLUT.Motion_Function (Mouse_Move_Handler'Access);
--        GLUT.Passive_Motion_Function (Mouse_Move_Handler'Access);
--        GLUT.Keyboard_Function (Key_Down_Handler'Access);
--        GLUT.Keyboard_Up_Function (Key_Up_Handler'Access);
--        GLUT.Special_Function (Special_Key_Handler'Access);

      GLUT.Idle_Function (Idle_Handler'Access);

      return Window : constant Rho.Windows.Window_Type :=
        new OpenGL_Window'
          (Rho.Windows.Root_Window_Type with
             Id => Window_Id)
      do
         Window.Initialize_Window (X, Y, Width, Height);

         if Load_Functions = 0 then
            raise Program_Error with "cannot load OpenGL";
         end if;

         Ada.Text_IO.Put_Line
           ("Rho-GL: "
            & Interfaces.C.Strings.Value
              (GL.Get_String (GL_Constants.GL_VENDOR))
            & ": "
            & Interfaces.C.Strings.Value
              (GL.Get_String (GL_Constants.GL_VERSION)));

         Handle.Windows.Append (Window);

--           GL.Polygon_Mode
--             (GL_Constants.GL_FRONT_AND_BACK,
--              GL_Constants.GL_LINE);

         GL.Enable (GL_Constants.GL_DEPTH_TEST);
--           GL.Depth_Function (GL_Constants.GL_LEQUAL);
--           GL.Clear_Depth (1.0);
--
         --  GL.Enable_Debug;

      end return;

   end Create_Window;

   ---------------------
   -- Display_Handler --
   ---------------------

   procedure Display_Handler is
   begin
      for Window of Local_Handle.Windows loop
         Window.Render;
      end loop;
      GL.Disable_Debug;
   end Display_Handler;

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle return Handle is
      use type Interfaces.C.unsigned;
   begin
      GLUT.Init;
      GLUT.Init_Display_Mode
        (Mode => GLUT.DOUBLE or GLUT.RGB or GLUT.DEPTH);

      Local_Handle.Assets := new OpenGL_Asset_Container;
      Local_Render_Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Preamble.Shader_Preamble (Vertex_Shader));
      Local_Render_Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Preamble.Shader_Preamble (Fragment_Shader));
      Local_Render_Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
           (Vertex_Shader, "camera", "mat4"));
      Local_Render_Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
           (Vertex_Shader, "model", "mat4"));
      Local_Render_Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Attributes.In_Attribute_Fragment
           (Vertex_Shader, "position", "vec3"));
      Local_Render_Target.Add_Shader_Fragment
        (Rho.Shaders.Slices.Main.Shader_Line
           (Stage    => Vertex_Shader,
            Priority => Rho.Shaders.Slices.Shader_Source_Priority'Last,
            Name     => "Apply all matrix transformations to position",
            Line     => "gl_Position = camera * model * vec4(position, 1)"));

      return Local_Handle'Access;
   end Get_Handle;

   ------------------
   -- Idle_Handler --
   ------------------

   procedure Idle_Handler is
   begin
      GLUT.Post_Redisplay;
   end Idle_Handler;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Texture : in out OpenGL_Texture_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
      use GL_Constants;
      use GL_Types;
      use Rho.Textures;

      Id           : array (1 .. 1) of aliased Uint;
      To_GL_Wrap   : constant array (Texture_Address_Mode) of GLenum :=
                       (Border => GL_REPEAT, Clamp => GL_CLAMP,
                        Mirror => GL_CLAMP, Wrap => GL_REPEAT);
      To_GL_Filter : constant array (Texture_Filter_Type) of GLenum :=
                       (Nearest => GL_NEAREST, Linear => GL_LINEAR);

      GL_S_Wrap     : constant GLenum :=
                        To_GL_Wrap (Texture.S_Border);
      GL_T_Wrap     : constant GLenum :=
                        To_GL_Wrap (Texture.T_Border);
      GL_Mag_Filter : constant GLenum :=
                        To_GL_Filter (Texture.Mag_Filter);
   begin
      Rho.Logging.Log ("loading texture: " & Texture.Name);
      GL.Gen_Textures (1, Id (Id'First)'Access);
      GL.Bind_Texture (GL_TEXTURE_2D, Id (1));

      GL.Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_S_Wrap);
      GL.Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_T_Wrap);
      GL.Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_Mag_Filter);
      GL.Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_Mag_Filter);
      Texture.Id := Natural (Id (1));

      declare
         use Glib;
         Width  : constant Gint :=
                    Cairo.Image_Surface.Get_Width (Texture.Surface);
         Height : constant Gint :=
                     Cairo.Image_Surface.Get_Height (Texture.Surface);
         Data : Aliased_Ubyte_Array_Access :=
                  To_Color_Array (Texture.Surface);
      begin
         if Width > 0 and then Height > 0 and then Data /= null then
            OpenGL_Render_Target_Access (Target).Load_Texture_Data
              (Texture_Id => Texture.Id,
               Width      => Positive (Width),
               Height     => Positive (Height),
               Data       => Data);
            Free (Data);
         end if;

      end;

      OpenGL_Render_Target_Access (Target).Active_Texture_Id := Texture.Id;

   end Load;

   -----------------
   -- Load_Buffer --
   -----------------

   overriding procedure Load_Buffer
     (Target  : in out OpenGL_Render_Target;
      Buffer  : Rho.Buffers.Buffer_Type)
   is

      procedure Load_Double_Buffer;
      procedure Load_Integer_Buffer;

      ------------------------
      -- Load_Double_Buffer --
      ------------------------

      procedure Load_Double_Buffer is
         use GL_Constants;
         use GL_Types;

         type Array_Of_Float_Access is
           access GL_Types.Array_Of_Float;

         procedure Free is
           new Ada.Unchecked_Deallocation
             (GL_Types.Array_Of_Float, Array_Of_Float_Access);

         GL_Values  : Array_Of_Float_Access :=
           new GL_Types.Array_Of_Float
             (1 .. Buffer.Scalar_Count);

         Count : Natural := 0;

         Buffer_Id  : aliased Uint;

         procedure Add_Value (X : Real);

         ---------------
         -- Add_Value --
         ---------------

         procedure Add_Value (X : Real) is
         begin
            Count := Count + 1;
            GL_Values (Count) := GLfloat (X);
         end Add_Value;

      begin

         Buffer.Iterate (Add_Value'Access);

         GL.Gen_Buffers (1, Buffer_Id'Access);
         GL.Bind_Buffer (GL_ARRAY_BUFFER, Buffer_Id);

         GL.Buffer_Data (GL_ARRAY_BUFFER,
                         GL_Values.all'Size / System.Storage_Unit,
                         GL_Values.all'Address, GL_STATIC_DRAW);

         Free (GL_Values);

         Target.Id_Map.Define_Buffer (Buffer, Buffer_Id);
      end Load_Double_Buffer;

      -------------------------
      -- Load_Integer_Buffer --
      -------------------------

      procedure Load_Integer_Buffer is
         use GL_Constants;
         use GL_Types;

         type Array_Of_Integer_Access is
           access GL_Types.Array_Of_Int;

         procedure Free is
           new Ada.Unchecked_Deallocation
             (GL_Types.Array_Of_Int, Array_Of_Integer_Access);

         GL_Values  : Array_Of_Integer_Access :=
           new GL_Types.Array_Of_Int
             (1 .. Buffer.Scalar_Count);

         Count : Natural := 0;
         Buffer_Id  : aliased Uint;

         procedure Add_Value (X : Integer);

         ---------------
         -- Add_Value --
         ---------------

         procedure Add_Value (X : Integer) is
         begin
            Count := Count + 1;
            GL_Values (Count) := Int (X);
         end Add_Value;

      begin

         Buffer.Iterate (Add_Value'Access);

         GL.Gen_Buffers (1, Buffer_Id'Access);
         GL.Bind_Buffer (GL_ELEMENT_ARRAY_BUFFER, Buffer_Id);

         GL.Buffer_Data (GL_ELEMENT_ARRAY_BUFFER,
                         GL_Values.all'Size / System.Storage_Unit,
                         GL_Values.all'Address, GL_STATIC_DRAW);

         Free (GL_Values);

         Target.Id_Map.Define_Buffer (Buffer, Buffer_Id);
      end Load_Integer_Buffer;

   begin

      if Buffer.Scalar_Count > 0 then
         case Buffer.Contents is
         when Rho.Buffers.Integer_Data =>
            Load_Integer_Buffer;
         when others =>
            Load_Double_Buffer;
         end case;
      end if;

   end Load_Buffer;

   -----------------------
   -- Load_Texture_Data --
   -----------------------

   procedure Load_Texture_Data
     (Target     : not null access OpenGL_Render_Target'Class;
      Texture_Id : Natural;
      Width      : Positive;
      Height     : Positive;
      Data       : Aliased_Ubyte_Array_Access)
   is
      use GL_Constants;
      use GL_Types;
   begin
      if Target.Active_Texture_Id /= Texture_Id then
         GL.Bind_Texture (GL_TEXTURE_2D, Uint (Texture_Id));
         Target.Active_Texture_Id := Texture_Id;
      end if;

      Rho.Logging.Log
        ("load texture data: id" & Texture_Id'Image
         & " size" & Width'Image & " x" & Height'Image);

      GL.Tex_Image_2D
        (Target          => GL_TEXTURE_2D,
         Level           => 0,
         Internal_Format => GL_RGBA,
         Width           => Sizei (Width),
         Height          => Sizei (Height),
         Border          => 0,
         Format          => GL_RGBA,
         Ptype           => GL_UNSIGNED_BYTE,
         Pixels          => Data (0)'Address);

   end Load_Texture_Data;

   ---------------
   -- Main_Loop --
   ---------------

   overriding procedure Main_Loop
     (Handle : in out OpenGL_Handle)
   is
      pragma Unreferenced (Handle);
   begin
      GLUT.Main_Loop;
   end Main_Loop;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (Path : String) return String is
      use Ada.Text_IO;
      File : File_Type;
      Opened : Boolean := False;

      function Read_Contents return String;

      -------------------
      -- Read_Contents --
      -------------------

      function Read_Contents return String is
      begin
         if End_Of_File (File) then
            return "";
         else
            return Get_Line (File) & Ada.Characters.Latin_1.LF
              & Read_Contents;
         end if;
      end Read_Contents;

   begin
      Open (File, In_File, Path);
      Opened := True;

      return Contents : constant String := Read_Contents do
         Close (File);
      end return;

   exception
      when others =>
         if Opened then
            Close (File);
         end if;
         raise;
   end Read_File;

   ----------------------------
   -- Render_Current_Buffers --
   ----------------------------

   overriding procedure Render_Current_Buffers
     (Target : in out OpenGL_Render_Target)
   is
   begin
      GL.Draw_Elements
        (Mode       => GL_Constants.GL_TRIANGLES,
         Count      => GL_Types.Sizei (Target.Current_Count),
         Index_Type => GL_Constants.GL_UNSIGNED_INT,
         Offset     => 0);
   end Render_Current_Buffers;

   ---------------------
   -- Reshape_Handler --
   ---------------------

   procedure Reshape_Handler
     (Width, Height : Integer)
   is
   begin
      null;
   end Reshape_Handler;

   ---------------------------
   -- Set_Model_View_Matrix --
   ---------------------------

   overriding procedure Set_Model_View_Matrix
     (Target : in out OpenGL_Render_Target;
      Matrix : Rho.Matrices.Matrix_4)
   is
   begin
      Target.Active_Model_View := Matrix;
   end Set_Model_View_Matrix;

   ---------------------------
   -- Set_Projection_Matrix --
   ---------------------------

   overriding procedure Set_Projection_Matrix
     (Target : in out OpenGL_Render_Target;
      Matrix : Rho.Matrices.Matrix_4)
   is
   begin
      Target.Active_Projection := Matrix;
   end Set_Projection_Matrix;

   --------------------
   -- To_Color_Array --
   --------------------

   function To_Color_Array
     (From_Surface : Cairo.Cairo_Surface)
      return Aliased_Ubyte_Array_Access
   is
      use Glib;
      use GL_Types;
      Data       : constant System.Address :=
                     Cairo.Image_Surface.Get_Data_Generic (From_Surface);
      Stride     : constant Glib.Gint :=
                     Cairo.Image_Surface.Get_Stride (From_Surface);
      Src_Width  : constant Gint :=
                     Cairo.Image_Surface.Get_Width (From_Surface);
      Src_Height : constant Gint :=
                     Cairo.Image_Surface.Get_Height (From_Surface);
      Start_X    : constant Gint := 0;
      Start_Y    : constant Gint := 0;
      Width      : constant Int := Int (Src_Width);
      Height     : constant Int := Int (Src_Height);

      type Source_Data_Array is array (Gint range <>) of aliased Ubyte;
      Source_Data : Source_Data_Array (0 .. Stride * Src_Height - 1);
      for Source_Data'Address use Data;
      Dest_Data : Aliased_Ubyte_Array_Access;

      procedure Set_Dest (Source_Index : Gint;
                          X, Y         : Int);

      --------------
      -- Set_Dest --
      --------------

      procedure Set_Dest (Source_Index : Gint;
                          X, Y         : Int)
      is
         Dest_Index : constant Int :=
                        4 * X + (Height - Y - 1) * Width * 4;
      begin
         if Source_Index < 0 then
            Dest_Data (Dest_Index .. Dest_Index + 3) := (others => 0);
         else
            declare
               Red   : constant Ubyte := Source_Data (Source_Index + 2);
               Green : constant Ubyte := Source_Data (Source_Index + 1);
               Blue  : constant Ubyte := Source_Data (Source_Index + 0);
               Alpha : constant Ubyte := Source_Data (Source_Index + 3);
            begin
               Dest_Data (Dest_Index) := Red;
               Dest_Data (Dest_Index + 1) := Green;
               Dest_Data (Dest_Index + 2) := Blue;
               Dest_Data (Dest_Index + 3) := Alpha;
            end;
         end if;
      end Set_Dest;

   begin
      Cairo.Surface.Flush (From_Surface);

      if Height = 0 or else Width = 0 then
         return null;
      end if;

      Dest_Data := new Aliased_Ubyte_Array (0 .. 4 * Width * Height - 1);

      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            declare
               Src_X        : constant Gint := Gint (X) + Start_X;
               Src_Y        : constant Gint := Gint (Y) + Start_Y;
               Source_Index : constant Gint :=
                                (if Src_X < Src_Width
                                 and then Src_Y < Src_Height
                                 then 4 * Src_X + Src_Y * Stride
                                 else -1);
            begin
               Set_Dest (Source_Index, X, Y);
            end;
         end loop;
      end loop;
      return Dest_Data;
   end To_Color_Array;

   -----------------------
   -- To_GL_Float_Array --
   -----------------------

   function To_GL_Float_Array
     (Matrix : Rho.Matrices.Matrix_4)
      return GL_Types.Float_Matrix_4x4
   is
      Xs : constant Rho.Real_Arrays.Real_Vector :=
        Rho.Matrices.Column_Major_Values (Matrix);
   begin
      return M : GL_Types.Float_Matrix_4x4 do
         for I in M'Range loop
            M (I) := GL_Types.GLfloat (Xs (I));
         end loop;
      end return;
   end To_GL_Float_Array;

end Rho.Handles.OpenGL;
