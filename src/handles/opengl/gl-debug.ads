with GL_Types;

package GL.Debug is

   function Image (Value : GL_Types.GLenum) return String;

   procedure Put (V : GL_Types.Array_Of_Float);
   procedure Put (V : GL_Types.GLfloat);

end GL.Debug;
