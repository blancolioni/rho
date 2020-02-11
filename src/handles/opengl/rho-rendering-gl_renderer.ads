private with Rho.Mouse;
private with GL_Types;

package Rho.Rendering.GL_Renderer is

   function Create_GL_Renderer
     return Rho_Renderer;

private

   type GL_Mouse_State_Record is
     new Rho.Mouse.Rho_Mouse_State_Record with
      record
         State : Rho.Mouse.Mouse_State;
      end record;

   overriding
   function State (Mouse : GL_Mouse_State_Record)
                   return Rho.Mouse.Mouse_State
   is (Mouse.State);

   type Gtk_Mouse_State is access all GL_Mouse_State_Record'Class;

   GL_Mouse : aliased GL_Mouse_State_Record;

   function GL_Error_To_String
     (Error : GL_Types.GLenum)
      return String;

   type Built_In_Shader is
     (Pass_Through_Shader,
      Texture_Shader);

end Rho.Rendering.GL_Renderer;
