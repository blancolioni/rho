with Rho.Matrices;

with Tau.Types.Scalar;
with Tau.Types.Vectors;

package body Tau.Values is

   type Real_Value_Record is
     new Root_Tau_Value with
      record
         Value : Rho.Real;
      end record;

   overriding function To_Source
     (Value : Real_Value_Record)
      return String
   is (Value.Value'Image);

   type Vector_4_Value_Record is
     new Root_Tau_Value with
      record
         Value : Rho.Matrices.Vector_4;
      end record;

   overriding function To_Source
     (Value : Vector_4_Value_Record)
      return String
   is ("vec4" & Rho.Matrices.Image (Value.Value));

   -----------
   -- Color --
   -----------

   function Color
     (Position         : GCS.Positions.File_Position;
      Red, Green, Blue : Rho.Unit_Real;
      Alpha            : Rho.Unit_Real := 1.0)
      return Tau_Value
   is
      Result : Vector_4_Value_Record :=
                 Vector_4_Value_Record'
                   (Root_Tau_Node with
                    Value_Type => Tau.Types.Vectors.Vector (4),
                    Value      =>
                      Rho.Matrices.To_Vector (Red, Green, Blue, Alpha));
   begin
      Result.Initialize_Node (Position);
      return new Vector_4_Value_Record'(Result);
   end Color;

   -----------
   -- Color --
   -----------

   function Color
     (Position  : GCS.Positions.File_Position;
      Rho_Color : Rho.Color.Color_Type)
      return Tau_Value
   is
   begin
      return Color (Position, Rho_Color.R, Rho_Color.G,
                    Rho_Color.B, Rho_Color.A);
   end Color;

   ----------------
   -- Real_Value --
   ----------------

   function Real_Value
     (Position : GCS.Positions.File_Position;
      Value    : Rho.Real)
      return Tau_Value
   is
      Result : Real_Value_Record := Real_Value_Record'
        (Root_Tau_Node with
         Value_Type => Tau.Types.Scalar.Tau_Float,
         Value      => Value);
   begin
      Result.Initialize_Node (Position);
      return new Real_Value_Record'(Result);
   end Real_Value;

end Tau.Values;
