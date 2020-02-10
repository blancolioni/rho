with Rho.Assets;
with Rho.Buffers;
with Rho.Matrices;
with Rho.Shaders;
with Rho.Signals;

package Rho.Render is

   type Render_Target is interface
     and Rho.Buffers.Buffer_Handler_Interface
     and Rho.Signals.Signal_Dispatch_Interface;

   procedure Set_Projection_Matrix
     (Render : in out Render_Target;
      Matrix : Rho.Matrices.Matrix_4)
   is abstract;

   procedure Set_Model_View_Matrix
     (Render : in out Render_Target;
      Matrix : Rho.Matrices.Matrix_4)
   is abstract;

   procedure Render_Current_Buffers
     (Render : in out Render_Target)
   is abstract;

   procedure Activate_Shader
     (Render : in out Render_Target;
      Shader : Rho.Shaders.Program_Type)
   is abstract;

   procedure Bind_Shader
     (Render : in out Render_Target;
      Shader : Rho.Shaders.Program_Type)
   is abstract;

   function Current_Shader
     (Render : Render_Target)
      return Rho.Shaders.Program_Type
      is abstract;

   function Assets
     (Render : Render_Target)
      return Rho.Assets.Asset_Container_Type
      is abstract;

end Rho.Render;
