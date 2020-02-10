with GL_Types;

package GL_Errors is

   function GL_Error_To_String
     (Error : GL_Types.GLenum)
      return String;

   procedure Put_Error (Error : GL_Types.GLenum);

end GL_Errors;
