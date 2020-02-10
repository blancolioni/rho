with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with System;
with Interfaces.C;
with Interfaces.C.Strings;

with GLUT;

with GL;
with GL_Constants;
with GL_Types;

with Tau.Environment;

with Rho.Buffers;
with Rho.Color;
with Rho.Matrices;
with Rho.Shaders;
with Rho.Signals;

with Rho.Real_Arrays;

with Rho.Handles.OpenGL.Maps;

package body Rho.Handles.OpenGL is

   function Read_File (Path : String) return String;

   type OpenGL_Asset_Container is
     new Rho.Assets.Root_Asset_Container_Type with
      record
         null;
      end record;

   overriding function Generator_Name
     (Container : OpenGL_Asset_Container)
      return String
   is ("glsl");

   overriding procedure Compile_Shader
     (Container : in out OpenGL_Asset_Container;
      Shader    : Rho.Shaders.Shader_Type);

   overriding function Create_Program
     (Container : in out OpenGL_Asset_Container;
      Name      : String;
      Shaders   : Rho.Shaders.Shader_Array)
      return Rho.Shaders.Program_Type;

   type OpenGL_Render_Target is
     new Rho.Signals.Signal_Dispatcher
     and Rho.Render.Render_Target with
      record
         Assets            : Rho.Assets.Asset_Container_Type;
         Active_Program    : Rho.Shaders.Program_Type;
         Active_Projection : Rho.Matrices.Matrix_4;
         Active_Model_View : Rho.Matrices.Matrix_4;
         Current_Count     : Natural := 0;
         Id_Map            : Rho.Handles.OpenGL.Maps.Id_Map;
      end record;

   overriding function Assets
     (Target : OpenGL_Render_Target)
      return Rho.Assets.Asset_Container_Type
   is (Target.Assets);

   overriding procedure Activate_Shader
     (Target : in out OpenGL_Render_Target;
      Shader : Rho.Shaders.Program_Type);

   overriding procedure Bind_Shader
     (Target : in out OpenGL_Render_Target;
      Shader : Rho.Shaders.Program_Type);

   overriding function Current_Shader
     (Target : OpenGL_Render_Target)
      return Rho.Shaders.Program_Type
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
      Argument : not null access Rho.Shaders.Root_Shader_Variable_Type'Class);

   Local_Render_Target : aliased OpenGL_Render_Target :=
     OpenGL_Render_Target'
       (Rho.Signals.Signal_Dispatcher with
        Assets            => new OpenGL_Asset_Container,
        others            => <>);

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

   ---------------------
   -- Activate_Buffer --
   ---------------------

   overriding procedure Activate_Buffer
     (Target   : in out OpenGL_Render_Target;
      Buffer   : Rho.Buffers.Buffer_Type;
      Argument : not null access Rho.Shaders.Root_Shader_Variable_Type'Class)
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
      Shader : Rho.Shaders.Program_Type)
   is
      use type Rho.Shaders.Program_Type;
   begin
      if Target.Active_Program /= Shader then
         GL.Use_Program
           (Target.Id_Map.Program_Id (Shader));
         Target.Active_Program := Shader;
      end if;
   end Activate_Shader;

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
   end Before_Render;

   -----------------
   -- Bind_Shader --
   -----------------

   overriding procedure Bind_Shader
     (Target : in out OpenGL_Render_Target;
      Shader : Rho.Shaders.Program_Type)
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
   end Bind_Shader;

   --------------------
   -- Compile_Shader --
   --------------------

   overriding procedure Compile_Shader
     (Container : in out OpenGL_Asset_Container;
      Shader    : Rho.Shaders.Shader_Type)
   is
      GL_Stage : constant GL_Types.GLenum :=
                   (case Shader.Stage is
                       when Vertex_Shader   =>
                         GL_Constants.GL_VERTEX_SHADER,
                       when Fragment_Shader =>
                         GL_Constants.GL_FRAGMENT_SHADER);
   begin
      if not Shader.Has_Source then
         declare
            Extension : constant String :=
                          (case Shader.Stage is
                              when Vertex_Shader   => "vert",
                              when Fragment_Shader => "frag");
            Path      : constant String :=
                          Container.Find_File (Shader.Name, Extension);
         begin
            if Path = "" then
               raise Constraint_Error with
                 "unabled to locate shader: " & Shader.Name;
            end if;

            declare
               Source : constant String := Read_File (Path);
            begin
               Shader.Set_Source (Source);
            end;
         end;
      end if;

      declare
         Source : constant String := Shader.Shader_Source;
         Id     : constant GL_Types.Uint :=
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
            if Status = 0 then
               GL.Get_Shader (Id, GL_INFO_LOG_LENGTH, Log_Length'Access);
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
               raise Constraint_Error with
               Shader.Name & " failed to compile";
            end if;
         end;

         Local_Render_Target.Id_Map.Define_Shader (Shader, Id);
         Shader.Set_Loaded;
      end;

   end Compile_Shader;

   --------------------
   -- Create_Program --
   --------------------

   overriding function Create_Program
     (Container : in out OpenGL_Asset_Container;
      Name      : String;
      Shaders   : Rho.Shaders.Shader_Array)
      return Rho.Shaders.Program_Type
   is
      pragma Unreferenced (Container);
      Id : constant GL_Types.Uint :=
        GL.Create_Program;
   begin
      for Shader of Shaders loop
         GL.Attach_Shader
           (Id, Local_Render_Target.Id_Map.Shader_Id (Shader));
      end loop;
      GL.Link_Program (Id);

      return Program : constant Rho.Shaders.Program_Type :=
        Rho.Shaders.Create_Program (Name)
      do
         declare
            procedure Bind_Variable
              (Variable : not null access
                 Rho.Shaders.Root_Shader_Variable_Type'Class);

            -------------------
            -- Bind_Variable --
            -------------------

            procedure Bind_Variable
              (Variable : not null access
                 Rho.Shaders.Root_Shader_Variable_Type'Class)
            is
               use GL_Types;
               Variable_Id : Int := 0;
            begin
               if Variable.Is_Attribute then
                  Variable_Id :=
                    GL.Get_Attribute_Location (Id, Variable.Name);
               elsif Variable.Is_Uniform then
                  Variable_Id :=
                    GL.Get_Uniform_Location (Id, Variable.Name);
               end if;
               if Variable_Id >= 0 then
                  Local_Render_Target.Id_Map.Define_Variable
                    (Variable, Variable_Id);
               end if;
            end Bind_Variable;

         begin
            Program.Iterate_Variables (Bind_Variable'Access);
         end;

         Local_Render_Target.Id_Map.Define_Program (Program, Id);
      end return;
   end Create_Program;

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
--           GL.Enable_Debug;

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

      return Local_Handle'Access;
   end Get_Handle;

   ------------------
   -- Idle_Handler --
   ------------------

   procedure Idle_Handler is
   begin
      GLUT.Post_Redisplay;
   end Idle_Handler;

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
