package body Rho.Matrices is

   function "*" (Left, Right : Vector_3) return Vector_3 is
      A : Real_Arrays.Real_Vector renames Left.Vector;
      B : Real_Arrays.Real_Vector renames Right.Vector;
   begin
      return (Vector => (A (2) * B (3) - A (3) - B (2),
                         A (3) * B (1) - A (1) - B (3),
                         A (1) * B (2) - A (2) - B (1)));
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Vector_3) return Real is
      A : Real_Arrays.Real_Vector renames Left.Vector;
      B : Real_Arrays.Real_Vector renames Right.Vector;
   begin
      return A (1) * B (1) + A (2) * B  (2) + A (3) * B (3);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Quaternion) return Quaternion is

      X1 : constant Real := Left.Vector (1);
      Y1 : constant Real := Left.Vector (2);
      Z1 : constant Real := Left.Vector (3);
      W1 : constant Real := Left.Vector (4);
      X2 : constant Real := Right.Vector (1);
      Y2 : constant Real := Right.Vector (2);
      Z2 : constant Real := Right.Vector (3);
      W2 : constant Real := Right.Vector (4);

      X  : constant Real := X1 * W2 + W1 * X2 + Y1 * Z2 - Z1 * Y2;
      Y  : constant Real := Y1 * W2 + W1 * Y2 + Z1 * X2 - X1 * Z2;
      Z  : constant Real := Z1 * W2 + W1 * Z2 + X1 * Y2 - Y1 * X2;
      W  : constant Real := W1 * W2 - X1 * X2 - Y1 * Y2 - Z1 * Z2;

--        Qax : constant Real := Left.Vector (1);
--        Qay : constant Real := Left.Vector (2);
--        Qaz : constant Real := Left.Vector (3);
--        Qaw : constant Real := Left.Vector (4);
--        Qbx : constant Real := Right.Vector (1);
--        Qby : constant Real := Right.Vector (2);
--        Qbz : constant Real := Right.Vector (3);
--        Qbw : constant Real := Right.Vector (4);
--
--        X : constant Real := Qax * Qbw + Qaw * Qbx + Qay * Qbz - Qaz * Qby;
--        Y : constant Real := Qay * Qbw + Qaw * Qby + Qaz * Qbx - Qax * Qbz;
--        Z : constant Real := Qaz * Qbw + Qaw * Qbz + Qax * Qby - Qay * Qbx;
--        W : constant Real := Qaw * Qbw - Qax * Qbx - Qay * Qby - Qaz * Qbz;

   begin
      return (Vector => (X, Y, Z, W));
   end "*";

   -----------------
   -- Apply_Scale --
   -----------------

   procedure Apply_Scale
     (Matrix  : in out Matrix_4;
      X, Y, Z : Non_Negative_Real)
   is
   begin
      Apply_Scale (Matrix, (Vector => (X, Y, Z)));
   end Apply_Scale;

   -----------------
   -- Apply_Scale --
   -----------------

   procedure Apply_Scale
     (Matrix  : in out Matrix_4;
      Scale   : Vector_3)
   is
   begin
      for I in Scale.Vector'Range loop
         Matrix.Matrix (I, I) := Matrix.Matrix (I, I) * Scale.Vector (I);
      end loop;
   end Apply_Scale;

   ---------------------------
   -- Axis_Angle_Quaternion --
   ---------------------------

   function Axis_Angle_Quaternion
     (Axis  : Vector_3;
      Angle : Real)
      return Quaternion
   is
      use Rho.Elementary_Functions;
      A : constant Real := Angle / 2.0;
      S : constant Signed_Unit_Real := Sin (A, 360.0);
      V : constant Real_Vector := S * Axis.Vector;
   begin
      return (Vector => V & Cos (A, 360.0));
   end Axis_Angle_Quaternion;

   -------------------------
   -- Column_Major_Values --
   -------------------------

   function Column_Major_Values
     (Matrix : Matrix_4)
      return Rho.Real_Arrays.Real_Vector
   is
   begin
      return Xs : Rho.Real_Arrays.Real_Vector (1 .. 16) do
         for Row in 1 .. 4 loop
            for Col in 1 .. 4 loop
               Xs ((Col - 1) * 4 + Row) := Matrix.Matrix (Row, Col);
            end loop;
         end loop;
      end return;
   end Column_Major_Values;

   -------------
   -- Compose --
   -------------

   function Compose
     (Position    : Vector_3;
      Orientation : Quaternion;
      Scale       : Vector_3)
      return Matrix_4
   is
   begin
      return Matrix : Matrix_4 := To_Matrix_4 (Orientation) do
         Set_Scale (Matrix, Scale);
         Set_Position (Matrix, Position);
      end return;
   end Compose;

   -----------
   -- Image --
   -----------

   function Image (Vector : Vector_4) return String is
      X : constant Real := Vector.Vector (1);
      Y : constant Real := Vector.Vector (2);
      Z : constant Real := Vector.Vector (3);
      W : constant Real := Vector.Vector (4);
   begin
      return "(" & X'Image & "," & Y'Image & "," & Z'Image
        & "," & W'Image & ")";
   end Image;

   -------------------------
   -- Orthographic_Matrix --
   -------------------------

   function Orthographic_Matrix
     (Left, Right  : Real;
      Bottom, Top  : Real)
      return Matrix_4
   is
      M_11 : constant Real := 2.0 / (Right - Left);
      M_14 : constant Real := -(Right + Left) / (Right - Left);
      M_22 : constant Real := 2.0 / (Top - Bottom);
      M_24 : constant Real := -(Top + Bottom) / (Top - Bottom);
      M_33 : constant Real := -1.0;
      M_34 : constant Real := 0.0;
   begin
      return (Matrix =>
                ((M_11, 0.0, 0.0, M_14),
                 (0.0, M_22, 0.0, M_24),
                 (0.0, 0.0, M_33, M_34),
                 (0.0, 0.0, 0.0, 1.0)));
   end Orthographic_Matrix;

   ------------------------
   -- Perspective_Matrix --
   ------------------------

   function Perspective_Matrix
     (Left, Right : Real;
      Bottom, Top : Real;
      Near, Far   : Real)
      return Matrix_4
   is
      X : constant Real := 2.0 * Near / (Right - Left);
      Y : constant Real := 2.0 * Near / (Top - Bottom);
      A : constant Real := (Right + Left) / (Right - Left);
      B : constant Real := (Top + Bottom) / (Top - Bottom);
      C : constant Real := -(Far + Near) / (Far - Near);
      D : constant Real := -2.0 * Far * Near / (Far - Near);
   begin
      return (Matrix =>
                ((X, 0.0, A, 0.0),
                 (0.0, Y, B, 0.0),
                 (0.0, 0.0, C, D),
                 (0.0, 0.0, -1.0, 0.0)));
   end Perspective_Matrix;

   ---------------------
   -- Rotation_Matrix --
   ---------------------

   function Rotation_Matrix
     (Axis  : Normal_Vector_3;
      Angle : Real)
      return Matrix_4
   is
   begin
      return To_Matrix_4 (Axis_Angle_Quaternion (Axis, Angle));
   end Rotation_Matrix;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Matrix   : in out Matrix_4;
      Position : Vector_3)
   is
   begin
      for I in Position.Vector'Range loop
         Matrix.Matrix (I, 4) := Position.Vector (I);
      end loop;
   end Set_Position;

   ---------------
   -- Set_Scale --
   ---------------

   procedure Set_Scale
     (Matrix : in out Matrix_4;
      Scale  : Vector_3)
   is
   begin
      for I in Scale.Vector'Range loop
         declare
            X : Real renames Matrix.Matrix (I, I);
         begin
            X := X * Scale.Vector (I);
         end;
      end loop;
   end Set_Scale;

   -----------------
   -- To_Matrix_4 --
   -----------------

   function To_Matrix_4
     (From : Quaternion)
      return Matrix_4
   is
      X : constant Real := From.Vector (1);
      Y : constant Real := From.Vector (2);
      Z : constant Real := From.Vector (3);
      W : constant Real := From.Vector (4);
      XX : constant Non_Negative_Real := X ** 2;
      YY : constant Non_Negative_Real := Y ** 2;
      ZZ : constant Non_Negative_Real := Z ** 2;
      XY : constant Real := X * Y;
      XZ : constant Real := X * Z;
      XW : constant Real := X * W;
      YZ : constant Real := Y * Z;
      YW : constant Real := Y * W;
      ZW : constant Real := Z * W;
   begin
      return (Matrix =>
                (
                 (1.0 - 2.0 * YY - 2.0 * ZZ,
                 2.0 * XY - 2.0 * ZW,
                 2.0 * XZ + 2.0 * YW,
                 0.0),
                 (2.0 * XY + 2.0 * ZW,
                  1.0 - 2.0 * XX - 2.0 * ZZ,
                  2.0 * YZ - 2.0 * XW,
                  0.0),
                 (2.0 * XZ - 2.0 * YW,
                  2.0 * YZ + 2.0 * XW,
                  1.0 - 2.0 * XX - 2.0 * YY,
                  0.0),
                 (0.0, 0.0, 0.0, 1.0)
                )
             );
   end To_Matrix_4;

   ------------------------
   -- Translation_Matrix --
   ------------------------

   function Translation_Matrix
     (Translation : Vector_3)
      return Matrix_4
   is
   begin
      return M : Matrix_4 := Unit do
         Set_Position (M, Translation);
      end return;
   end Translation_Matrix;

end Rho.Matrices;
