package Rho.Matrices.Logs is

   procedure Log_Matrix
     (Name   : String;
      Matrix : Matrix_4);

   procedure Log_Transformation
     (Name   : String;
      From   : Vector_4;
      To     : Vector_4);

   procedure Log_State
     (Name     : String;
      Position : Vector_3;
      Rotation : Quaternion;
      Scale    : Vector_3;
      Matrix   : Matrix_4);

   procedure Put_Vector (Vector : Vector_3);
   procedure Put_Vector (Vector : Vector_4);
   procedure Put_Quaternion (Q : Quaternion);

   procedure Put_Number (X : Real);

end Rho.Matrices.Logs;
