with Ada.Text_IO, Ada.Float_Text_IO;

with GL_Constants;

package body GL.Debug is

   function Hex_Image (Value : GL_Types.Uint) return String;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Value : GL_Types.Uint) return String is
      Result : String (1 .. 8);
      It     : Uint := Value;

      function Hex_Digit (X : Uint) return Character
      is ((if X < 10
           then Character'Val (48 + X)
           else Character'Val (55 + X)));

   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digit (It mod 16);
         It := It / 16;
      end loop;

      if Value < 65536 then
         return Result (5 .. 8);
      else
         return Result (1 .. 4) & "_" & Result (5 .. 8);
      end if;
   end Hex_Image;

   -----------
   -- Image --
   -----------

   function Image (Value : GL_Types.GLenum) return String is
      use GL_Constants;
   begin
      case Value is
         when GL_POINTS =>
            return "GL_POINTS";
         when GL_LINES =>
            return "GL_LINES";
         when GL_LINE_LOOP =>
            return "GL_LINE_LOOP";
         when GL_LINE_STRIP =>
            return "GL_LINE_STRIP";
         when GL_TRIANGLES =>
            return "GL_TRIANGLES";
         when GL_TRIANGLE_STRIP =>
            return "GL_TRIANGLE_STRIP";
         when GL_TRIANGLE_FAN =>
            return "GL_TRIANGLE_FAN";
         when GL_QUADS =>
            return "GL_QUADS";
         when GL_ARRAY_BUFFER =>
            return "GL_ARRAY_BUFFER";
         when GL_ELEMENT_ARRAY_BUFFER =>
            return "GL_ELEMENT_ARRAY_BUFFER";
         when GL_FLOAT =>
            return "GL_FLOAT";
         when GL_DOUBLE =>
            return "GL_DOUBLE";
         when GL_UNSIGNED_INT =>
            return "GL_UNSIGNED_INT";
         when GL_STREAM_DRAW =>
            return "GL_STREAM_DRAW";
         when GL_STREAM_READ =>
            return "GL_STREAM_READ";
         when GL_STREAM_COPY =>
            return "GL_STREAM_COPY";
         when GL_STATIC_DRAW =>
            return "GL_STATIC_DRAW";
         when GL_STATIC_READ =>
            return "GL_STATIC_READ";
         when GL_STATIC_COPY =>
            return "GL_DYNAMIC_COPY";
         when GL_DYNAMIC_DRAW =>
            return "GL_DYNAMIC_DRAW";
         when GL_DYNAMIC_READ =>
            return "GL_DYNAMIC_READ";
         when GL_DYNAMIC_COPY =>
            return "GL_STREAM_COPY";
         when GL_TEXTURE_1D =>
            return "GL_TEXTURE_1D";
         when GL_TEXTURE_2D =>
            return "GL_TEXTURE_2D";
         when GL_TEXTURE_CUBE_MAP =>
            return "GL_TEXTURE_CUBE_MAP";
         when GL_INVALID_OPERATION =>
            return "GL_INVALID_OPERATION";
         when others =>
            return Hex_Image (GL_Types.Uint (Value));
      end case;
   end Image;

   ---------
   -- Put --
   ---------

   procedure Put (V : GL_Types.GLfloat) is
   begin
      Ada.Float_Text_IO.Put (Float (V), 1, 4, 0);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (V : Array_Of_Float) is
      use Ada.Text_IO;
      First : Boolean := True;
   begin
      for X of V loop
         if First then
            Put ("(");
            First := False;
         else
            Put (",");
         end if;
         Ada.Float_Text_IO.Put (Float (X), 1, 4, 0);
      end loop;
      Put (")");
   end Put;

end GL.Debug;
