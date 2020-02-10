package Rho.Color is

   type Color_Type is
      record
         R, G, B, A : Unit_Real;
      end record;

   White : constant Color_Type := (1.0, 1.0, 1.0, 1.0);
   Black : constant Color_Type := (0.0, 0.0, 0.0, 1.0);

   function To_Shader_Value (Color : Color_Type) return String;

private

   function To_Shader_Value (Color : Color_Type) return String
   is ("vec4(" & Color.R'Image & "," & Color.G'Image
       & "," & Color.B'Image & "," & Color.A'Image & ")");

end Rho.Color;
