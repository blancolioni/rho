with Ada.Text_IO;

with GL_Constants;

package body GL_Errors is

   ------------------------
   -- GL_Error_To_String --
   ------------------------

   function GL_Error_To_String
     (Error : GL_Types.GLenum)
      return String
   is
      use GL_Constants, GL_Types;
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

   ---------------
   -- Put_Error --
   ---------------

   procedure Put_Error (Error : GL_Types.GLenum) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "error: " & GL_Error_To_String (Error));
   end Put_Error;

end GL_Errors;
