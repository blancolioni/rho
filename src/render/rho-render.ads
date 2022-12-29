with Rho.Assets;
with Rho.Buffers;
with Rho.Matrices;
with Rho.Shaders.Programs;
with Rho.Shaders.Stages;
with Rho.Shaders.Variables;
with Rho.Signals;
with Rho.Values;

--  with Tau.Generators;
--  with Tau.Shaders;

package Rho.Render is

   type Render_Target is interface
     and Rho.Buffers.Buffer_Handler_Interface
     and Rho.Signals.Signal_Dispatch_Interface;

   --  function Active_Shader_Slices
   --    (Target : Render_Target)
   --     return Rho.Shaders.Slices.Slice_Array
   --     is abstract;

   --  procedure Add_Shader_Fragment
   --    (Target   : in out Render_Target;
   --     Slice : Rho.Shaders.Slices.Slice_Type)
   --  is abstract;

   procedure Add_Shader
     (Target : in out Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type)
   is null;

   type Active_Shader_Array is
     array (Positive range <>) of Rho.Shaders.Programs.Program_Type;

   function Active_Shaders
     (Target : Render_Target)
      return Active_Shader_Array
      is abstract;

   procedure Set_Uniform
     (Target  : in out Render_Target;
      Name    : String;
      Value   : Rho.Values.Rho_Value)
   is abstract;

   procedure Set_Projection_Matrix
     (Render : in out Render_Target;
      Matrix : Rho.Matrices.Matrix_4)
   is abstract;

   procedure Set_Model_View_Matrix
     (Render : in out Render_Target;
      Matrix : Rho.Matrices.Matrix_4)
   is abstract;

   procedure Set_Camera_World_Matrix
     (Render : in out Render_Target;
      Matrix : Rho.Matrices.Matrix_4)
      is abstract;

   procedure Set_Camera_Position
     (Render   : in out Render_Target;
      Position : Rho.Matrices.Vector_3)
   is abstract;

   procedure Set_Size
     (Render : in out Render_Target;
      Width  : Natural;
      Height : Natural)
   is abstract;

   procedure Render_Current_Buffers
     (Render : in out Render_Target)
   is abstract;

   procedure Compile_Shader
     (Render : in out Render_Target;
      Shader : Rho.Shaders.Stages.Shader_Type)
   is abstract;

   function Create_Program
     (Render    : in out Render_Target;
      Name      : String;
      Shaders   : Rho.Shaders.Stages.Shader_Array)
      return Rho.Shaders.Programs.Program_Type
      is abstract;

   procedure Bind_Variable
     (Render   : in out Render_Target;
      Program  : Rho.Shaders.Programs.Program_Type;
      Variable : Rho.Shaders.Variables.Variable_Type)
   is abstract;

   procedure Activate_Shader
     (Render : in out Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type)
   is abstract;

   procedure Bind_Shader
     (Render : in out Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type)
   is abstract;

   function Current_Shader
     (Render : Render_Target)
      return Rho.Shaders.Programs.Program_Type
      is abstract;

   --  function Generator
   --    (Render : Render_Target)
   --     return Tau.Generators.Root_Tau_Generator'Class
   --  is abstract;

   function Assets
     (Render : Render_Target)
      return Rho.Assets.Asset_Container_Type
      is abstract;

end Rho.Render;
