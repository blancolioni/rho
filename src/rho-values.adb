package body Rho.Values is

   -------------------
   -- Default_Value --
   -------------------

   function Default_Value (Of_Type : Value_Type) return Rho_Value is
      Null_US : constant Ada.Strings.Unbounded.Unbounded_String :=
                  Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      case Of_Type is
         when String_Value =>
            return (String_Value, Null_US);
         when Identifier_Value =>
            return (Identifier_Value, Null_US);
         when Real_Value =>
            return (Real_Value, 0.0);
         when Vector_2_Value =>
            return (Vector_2_Value, Rho.Matrices.Zero);
         when Vector_3_Value =>
            return (Vector_3_Value, Rho.Matrices.Zero);
         when Vector_4_Value =>
            return (Vector_4_Value, Rho.Matrices.Zero);
         when Matrix_3_Value =>
            return (Matrix_3_Value, Rho.Matrices.Zero);
         when Matrix_4_Value =>
            return (Matrix_4_Value, Rho.Matrices.Zero);
         when Color_Value =>
            return Color_Value (Rho.Color.Black);
      end case;
   end Default_Value;

end Rho.Values;
