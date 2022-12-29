with Ada.Text_IO;
with Rho.Matrices.Logs;

package body Rho.Matrices is

   function "*" (Left, Right : Vector_3) return Vector_3 is
      A : Real_Arrays.Real_Vector renames Left.Vector;
      B : Real_Arrays.Real_Vector renames Right.Vector;
   begin
      return (Vector => (A (2) * B (3) - A (3) * B (2),
                         A (3) * B (1) - A (1) * B (3),
                         A (1) * B (2) - A (2) * B (1)));
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

   --------------------------
   -- From_Rotation_Matrix --
   --------------------------

   function From_Rotation_Matrix
     (Matrix : Matrix_3)
      return Quaternion
   is
      use Rho.Elementary_Functions;
      M : Real_Matrix renames Matrix.Matrix;
      M_11 : constant Real := M (1, 1);
      M_21 : constant Real := M (2, 1);
      M_31 : constant Real := M (3, 1);
      M_12 : constant Real := M (1, 2);
      M_22 : constant Real := M (2, 2);
      M_32 : constant Real := M (3, 2);
      M_13 : constant Real := M (1, 3);
      M_23 : constant Real := M (2, 3);
      M_33 : constant Real := M (3, 3);
      Trace : constant Real := M_11 + M_22 + M_33;
   begin
      if Trace > 0.0 then
         declare
            S : constant Real := 0.5 / Sqrt (Trace + 1.0);
         begin
            return (Vector =>
                      (0.25 / S,
                       (M_32 - M_23) * S,
                       (M_13 - M_31) * S,
                       (M_21 - M_12) * S));
         end;
      elsif M_11 > M_22 and then M_11 > M_33 then
         declare
            S : constant Real := 2.0 * Sqrt (1.0 + M_11 - M_22 - M_33);
         begin
            return (Vector =>
                      ((M_32 - M_23) / S,
                       0.25 * S,
                       (M_21 + M_12) / S,
                       (M_13 + M_31) / S
                      ));
         end;
      elsif M_22 > M_33 then
         declare
            S : constant Real := 2.0 * Sqrt (1.0 + M_22 - M_11 - M_33);
         begin
            return (Vector =>
                      (
                       (M_13 - M_31) / S,
                       (M_12 + M_21) / S,
                       0.25 * S,
                       (M_23 + M_32) / S
                      ));
         end;
      else
         declare
            S : constant Real := 2.0 * Sqrt (1.0 + M_33 - M_11 - M_33);
         begin
            return (Vector =>
                      (
                       (M_21 - M_12) / S,
                       (M_13 + M_31) / S,
                       (M_23 + M_32) / S,
                       0.25 * S
                      ));
         end;
      end if;
   end From_Rotation_Matrix;

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

   -----------
   -- Image --
   -----------

   function Image (Vector : Vector_3) return String is
      X : constant Real := Vector.Vector (1);
      Y : constant Real := Vector.Vector (2);
      Z : constant Real := Vector.Vector (3);
   begin
      return "(" & X'Image & "," & Y'Image & "," & Z'Image & ")";
   end Image;

   --------------------
   -- Look_At_Matrix --
   --------------------

   function Look_At_Matrix
     (From, To, Up : Vector_3)
      return Matrix_4
   is
      X, Y, Z : Vector_3;
   begin

      Z := From - To;

      if abs Z = 0.0 then
         Z.Vector (3) := 1.0;
      end if;
      Z := Normalize (Z);

      X := Up * Z;

      if abs X = 0.0 then
         if abs Up.Vector (3) = 1.0 then
            Z.Vector (1) := @ + 0.0001;
         else
            Z.Vector (3) := @ + 0.0001;
         end if;
         Z := Normalize (Z);
         X := Up * Z;
      end if;

      X := Normalize (X);

      Y := Z * X;

      if False then
         declare
            use Ada.Text_IO;
            use Rho.Matrices.Logs;
         begin
            Put ("from ");
            Put_Vector (From);
            Put (" to ");
            Put_Vector (To);
            Put (" up ");
            Put_Vector (Up);
            New_Line;
            Put ("x ");
            Put_Vector (X);
            Put (" y ");
            Put_Vector (Y);
            Put (" z ");
            Put_Vector (Z);
            New_Line;
         end;
      end if;

      return M : constant Rho.Matrices.Matrix_4 :=
        ((Matrix =>
              ((X.Vector (1), Y.Vector (1), Z.Vector (1), 0.0),
               (X.Vector (2), Y.Vector (2), Z.Vector (2), 0.0),
               (X.Vector (3), Y.Vector (3), Z.Vector (3), 0.0),
               (0.0, 0.0, 0.0, 1.0))))
      do
         if False then
            Rho.Matrices.Logs.Log_Matrix ("look at", M);
            Ada.Text_IO.Put ("quaternion ");
            Rho.Matrices.Logs.Put_Quaternion (From_Rotation_Matrix (M));
            Ada.Text_IO.New_Line;
         end if;
      end return;

   end Look_At_Matrix;
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
   -- To_Matrix_3 --
   -----------------

   function To_Matrix_3 (Matrix : Matrix_4) return Matrix_3 is
      M : Real_Arrays.Real_Matrix renames Matrix.Matrix;
   begin
      return (Matrix =>
                ((M (1, 1), M (1, 2), M (1, 3)),
                 (M (2, 1), M (2, 2), M (2, 3)),
                 (M (3, 1), M (3, 2), M (3, 3))));
   end To_Matrix_3;

   -----------------
   -- To_Matrix_4 --
   -----------------

   function To_Matrix_4 (Matrix : Matrix_3) return Matrix_4 is
      M : Real_Arrays.Real_Matrix renames Matrix.Matrix;
   begin
      return (Matrix =>
                ((M (1, 1), M (1, 2), M (1, 3), 0.0),
                 (M (2, 1), M (2, 2), M (2, 3), 0.0),
                 (M (3, 1), M (3, 2), M (3, 3), 0.0),
                 (0.0, 0.0, 0.0, 1.0)));
   end To_Matrix_4;

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
