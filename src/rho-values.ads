private with Ada.Strings.Unbounded;

with Rho.Color;
with Rho.Matrices;

package Rho.Values is

   type Value_Type is
     (String_Value,
      Identifier_Value,
      Real_Value,
      Vector_2_Value,
      Vector_3_Value,
      Vector_4_Value,
      Matrix_3_Value,
      Matrix_4_Value,
      Color_Value);

   type Rho_Value (Of_Type : Value_Type) is private;

   function Default_Value
     (Of_Type : Value_Type)
      return Rho_Value;

   function Real_Value
     (Value : Real)
      return Rho_Value;

   function Vector_Value
     (Value : Rho.Matrices.Vector_2)
      return Rho_Value;

   function Vector_Value
     (Value : Rho.Matrices.Vector_3)
      return Rho_Value;

   function Vector_Value
     (Value : Rho.Matrices.Vector_4)
      return Rho_Value;

   function Color_Value
     (Color : Rho.Color.Color_Type)
      return Rho_Value;

   function Color_Value
     (Red, Green, Blue : Unit_Real;
      Alpha            : Unit_Real := 1.0)
      return Rho_Value;

   function To_Real (Value : Rho_Value) return Real;
   function To_Vector_2 (Value : Rho_Value) return Rho.Matrices.Vector_2;
   function To_Vector_3 (Value : Rho_Value) return Rho.Matrices.Vector_3;
   function To_Vector_4 (Value : Rho_Value) return Rho.Matrices.Vector_4;

private

   type Rho_Value (Of_Type : Value_Type) is
      record
         case Of_Type is
            when String_Value =>
               String_Val : Ada.Strings.Unbounded.Unbounded_String;
            when Identifier_Value =>
               Identifier_Val : Ada.Strings.Unbounded.Unbounded_String;
            when Real_Value =>
               Real_Val  : Real;
            when Vector_2_Value =>
               Vector_2_Val : Rho.Matrices.Vector_2;
            when Vector_3_Value =>
               Vector_3_Val : Rho.Matrices.Vector_3;
            when Vector_4_Value | Color_Value =>
               Vector_4_Val : Rho.Matrices.Vector_4;
            when Matrix_3_Value =>
               Matrix_3_Val   : Rho.Matrices.Matrix_3;
            when Matrix_4_Value =>
               Matrix_4_Val   : Rho.Matrices.Matrix_4;
         end case;
      end record;

   function Real_Value
     (Value : Real)
      return Rho_Value
   is (Real_Value, Value);

   function Vector_Value
     (Value : Rho.Matrices.Vector_2)
      return Rho_Value
   is (Vector_2_Value, Value);

   function Vector_Value
     (Value : Rho.Matrices.Vector_3)
      return Rho_Value
   is (Vector_3_Value, Value);

   function Vector_Value
     (Value : Rho.Matrices.Vector_4)
      return Rho_Value
   is (Vector_4_Value, Value);

   function Color_Value
     (Color : Rho.Color.Color_Type)
      return Rho_Value
   is (Color_Value,
       Rho.Matrices.To_Vector (Color.R, Color.G, Color.B, Color.A));

   function Color_Value
     (Red, Green, Blue : Unit_Real;
      Alpha            : Unit_Real := 1.0)
      return Rho_Value
   is (Color_Value, Rho.Matrices.To_Vector (Red, Green, Blue, Alpha));

   function To_Real (Value : Rho_Value) return Real is (Value.Real_Val);

   function To_Vector_2 (Value : Rho_Value) return Rho.Matrices.Vector_2
   is (Value.Vector_2_Val);

   function To_Vector_3 (Value : Rho_Value) return Rho.Matrices.Vector_3
   is (Value.Vector_3_Val);

   function To_Vector_4 (Value : Rho_Value) return Rho.Matrices.Vector_4
   is (Value.Vector_4_Val);

end Rho.Values;
