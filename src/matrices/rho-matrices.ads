private with Rho.Elementary_Functions;
with Rho.Real_Arrays;

package Rho.Matrices is

   type Matrix_4 is private;

   function Zero return Matrix_4;
   function Unit return Matrix_4;

   function Orthographic_Matrix
     (Left, Right  : Real;
      Bottom, Top  : Real)
      return Matrix_4;

   function Perspective_Matrix
     (Left, Right : Real;
      Bottom, Top : Real;
      Near, Far   : Real)
      return Matrix_4;

   function Inverse
     (Matrix : Matrix_4)
      return Matrix_4;

   function Column_Major_Values
     (Matrix : Matrix_4)
      return Rho.Real_Arrays.Real_Vector;

   function "*" (Left, Right : Matrix_4) return Matrix_4;

   type Matrix_3 is private;

   function Zero return Matrix_3;
   function Unit return Matrix_3;

   function To_Matrix_3 (Matrix : Matrix_4) return Matrix_3;
   function To_Matrix_4 (Matrix : Matrix_3) return Matrix_4;

   function "*" (Left, Right : Matrix_3) return Matrix_3;

   type Vector_4 is private;

   function "*" (Left : Matrix_4; Right : Vector_4) return Vector_4;
   function "*" (Left : Vector_4; Right : Real) return Vector_4;
   function "/" (Left : Vector_4; Right : Real) return Vector_4
     with Pre => Right /= 0.0;

   function Image (Vector : Vector_4) return String;

   type Vector_2 is private;

   function X (Vector : Vector_2) return Real;
   function Y (Vector : Vector_2) return Real;

   type Vector_3 is private;

   function Unit_X return Vector_3;
   function Unit_Y return Vector_3;
   function Unit_Z return Vector_3;

   function X (Vector : Vector_3) return Real;
   function Y (Vector : Vector_3) return Real;
   function Z (Vector : Vector_3) return Real;

   function "abs" (Vector : Vector_3) return Non_Negative_Real;

   function "+" (Left, Right : Vector_3) return Vector_3;
   function "-" (Left, Right : Vector_3) return Vector_3;

   function "*" (Left : Real; Right : Vector_3) return Vector_3;
   function "*" (Left : Vector_3; Right : Real) return Vector_3;
   function "/" (Left : Vector_3; Right : Real) return Vector_3
     with Pre => Right /= 0.0;

   function "*" (Left, Right : Vector_3) return Vector_3;
   function "*" (Left, Right : Vector_3) return Real;

   function Image (Vector : Vector_3) return String;

   function X (Vector : Vector_4) return Real;
   function Y (Vector : Vector_4) return Real;
   function Z (Vector : Vector_4) return Real;
   function W (Vector : Vector_4) return Real;

   function "abs" (Vector : Vector_4) return Non_Negative_Real;

   function XYZ (Vector : Vector_4) return Vector_3;

   subtype Normal_Vector_3 is Vector_3;

   function Zero return Vector_2;
   function Zero return Vector_3;
   function Zero return Vector_4;

   function Normalize (Vector : Vector_3) return Normal_Vector_3;

   function To_Vector (X, Y : Real) return Vector_2;
   function To_Vector (X, Y, Z : Real) return Vector_3;
   function To_Vector (X, Y, Z, W : Real) return Vector_4;
   function To_Vector (V : Vector_3; W : Real) return Vector_4;

   function Rotation_Matrix
     (Axis  : Normal_Vector_3;
      Angle : Real)
      return Matrix_4;

   function Look_At_Matrix
     (From, To, Up : Vector_3)
      return Matrix_4;

   function Translation_Matrix
     (Translation : Vector_3)
      return Matrix_4;

   type Quaternion is private;

   function "*" (Left, Right : Quaternion) return Quaternion;

   function Axis_Angle_Quaternion
     (Axis  : Vector_3;
      Angle : Real)
      return Quaternion;

   function Look_At_Quaternion
     (From, To, Up : Vector_3)
      return Quaternion;

   function To_Matrix_4
     (From : Quaternion)
      return Matrix_4;

   function From_Rotation_Matrix
     (Matrix : Matrix_4)
      return Quaternion;

   function From_Rotation_Matrix
     (Matrix : Matrix_3)
      return Quaternion;

   function Compose
     (Position    : Vector_3;
      Orientation : Quaternion;
      Scale       : Vector_3)
      return Matrix_4;

   function Get_Position
     (Matrix : Matrix_4)
      return Vector_3;

   procedure Set_Position
     (Matrix   : in out Matrix_4;
      Position : Vector_3);

   procedure Set_Scale
     (Matrix : in out Matrix_4;
      Scale  : Vector_3);

   procedure Apply_Scale
     (Matrix  : in out Matrix_4;
      X, Y, Z : Non_Negative_Real);

   procedure Apply_Scale
     (Matrix  : in out Matrix_4;
      Scale   : Vector_3);

private

   use Rho.Real_Arrays;

   type Matrix_4 is
      record
         Matrix : Real_Arrays.Real_Matrix (1 .. 4, 1 .. 4) :=
           Real_Arrays.Unit_Matrix (4);
      end record;

   function Zero return Matrix_4
   is (Matrix => (others => (others => 0.0)));

   function Unit return Matrix_4
   is (Matrix => Unit_Matrix (4));

   function "*" (Left, Right : Matrix_4) return Matrix_4
   is (Matrix => Left.Matrix * Right.Matrix);

   function Inverse
     (Matrix : Matrix_4)
      return Matrix_4
   is (Matrix => Inverse (Matrix.Matrix));

   type Matrix_3 is
      record
         Matrix : Real_Arrays.Real_Matrix (1 .. 3, 1 .. 3) :=
           Real_Arrays.Unit_Matrix (3);
      end record;

   function Zero return Matrix_3
   is (Matrix => (others => (others => 0.0)));

   function Unit return Matrix_3
   is (Matrix => Unit_Matrix (3));

   function "*" (Left, Right : Matrix_3) return Matrix_3
   is (Matrix => Left.Matrix * Right.Matrix);

   type Vector_4 is
      record
         Vector : Real_Arrays.Real_Vector (1 .. 4) := (0.0, 0.0, 0.0, 1.0);
      end record;

   function "*" (Left : Matrix_4; Right : Vector_4) return Vector_4
   is (Vector => Left.Matrix * Right.Vector);

   function "*" (Left : Vector_4; Right : Real) return Vector_4
   is (Vector => Left.Vector * Right);

   function "/" (Left : Vector_4; Right : Real) return Vector_4
   is (Vector => Left.Vector / Right);

   function "abs" (Vector : Vector_4) return Non_Negative_Real
   is (Rho.Elementary_Functions.Sqrt
       (Vector.Vector (1) ** 2
        + Vector.Vector (2) ** 2
        + Vector.Vector (3) ** 2
        + Vector.Vector (4) ** 2));

   type Vector_3 is
      record
         Vector : Real_Arrays.Real_Vector (1 .. 3) := (0.0, 0.0, 0.0);
      end record;

   function "abs" (Vector : Vector_3) return Non_Negative_Real
   is (Rho.Elementary_Functions.Sqrt
       (Vector.Vector (1) ** 2
        + Vector.Vector (2) ** 2
        + Vector.Vector (3) ** 2));

   function "+" (Left, Right : Vector_3) return Vector_3
   is (Vector => (Left.Vector (1) + Right.Vector (1),
                  Left.Vector (2) + Right.Vector (2),
                  Left.Vector (3) + Right.Vector (3)));

   function "-" (Left, Right : Vector_3) return Vector_3
   is (Vector => (Left.Vector (1) - Right.Vector (1),
                  Left.Vector (2) - Right.Vector (2),
                  Left.Vector (3) - Right.Vector (3)));

   function "*" (Left : Real; Right : Vector_3) return Vector_3
   is (Vector => (Left * Right.Vector (1),
                  Left * Right.Vector (2),
                  Left * Right.Vector (3)));

   function "*" (Left : Vector_3; Right : Real) return Vector_3
   is (Vector => (Left.Vector (1) * Right,
                  Left.Vector (2) * Right,
                  Left.Vector (3) * Right));

   function "/" (Left : Vector_3; Right : Real) return Vector_3
   is (Vector => (Left.Vector (1) / Right,
                  Left.Vector (2) / Right,
                  Left.Vector (3) / Right));

   type Vector_2 is
      record
         Vector : Real_Arrays.Real_Vector (1 .. 2) := (0.0, 0.0);
      end record;

   function Zero return Vector_2 is (Vector => (0.0, 0.0));
   function Zero return Vector_3 is (Vector => (0.0, 0.0, 0.0));
   function Zero return Vector_4 is (Vector => (0.0, 0.0, 0.0, 1.0));

   function Unit_X return Vector_3 is (Vector => (1.0, 0.0, 0.0));
   function Unit_Y return Vector_3 is (Vector => (0.0, 1.0, 0.0));
   function Unit_Z return Vector_3 is (Vector => (0.0, 0.0, 1.0));

   function Normalize (Vector : Vector_3) return Vector_3
   is (Vector => Real_Arrays."/" (Vector.Vector,
                                  Real_Arrays."abs" (Vector.Vector)));

   function To_Vector (X, Y : Real) return Vector_2
   is (Vector => (X, Y));

   function To_Vector (X, Y, Z : Real) return Vector_3
   is (Vector => (X, Y, Z));

   function To_Vector (X, Y, Z, W : Real) return Vector_4
   is (Vector => (X, Y, Z, W));

   function To_Vector (V : Vector_3; W : Real) return Vector_4
   is (Vector => (V.Vector (1), V.Vector (2), V.Vector (3), W));

   function X (Vector : Vector_4) return Real is (Vector.Vector (1));
   function Y (Vector : Vector_4) return Real is (Vector.Vector (2));
   function Z (Vector : Vector_4) return Real is (Vector.Vector (3));
   function W (Vector : Vector_4) return Real is (Vector.Vector (4));

   function XYZ (Vector : Vector_4) return Vector_3 is
     (Vector => Vector.Vector (1 .. 3));

   function X (Vector : Vector_3) return Real is (Vector.Vector (1));
   function Y (Vector : Vector_3) return Real is (Vector.Vector (2));
   function Z (Vector : Vector_3) return Real is (Vector.Vector (3));

   function X (Vector : Vector_2) return Real is (Vector.Vector (1));
   function Y (Vector : Vector_2) return Real is (Vector.Vector (2));

   function Get_Position
     (Matrix : Matrix_4)
      return Vector_3
   is (Vector => (Matrix.Matrix (1, 4),
                  Matrix.Matrix (2, 4),
                  Matrix.Matrix (3, 4)));

   type Quaternion is
      record
         Vector : Real_Arrays.Real_Vector (1 .. 4) := (0.0, 0.0, 0.0, 1.0);
      end record;

   function Look_At_Quaternion
     (From, To, Up : Vector_3)
      return Quaternion
   is (From_Rotation_Matrix (Look_At_Matrix (From, To, Up)));

   function From_Rotation_Matrix
     (Matrix : Matrix_4)
      return Quaternion
   is (From_Rotation_Matrix (To_Matrix_3 (Matrix)));

end Rho.Matrices;
