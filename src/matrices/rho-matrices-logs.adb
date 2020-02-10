with Ada.Text_IO;

package body Rho.Matrices.Logs is

   package Real_IO is new Ada.Text_IO.Float_IO (Real);

   ----------------
   -- Log_Matrix --
   ----------------

   procedure Log_Matrix
     (Name   : String;
      Matrix : Matrix_4)
   is
   begin
      if Name /= "" then
         Ada.Text_IO.Put_Line (Name);
      end if;

      for Col in 1 .. 4 loop
         Ada.Text_IO.Put ("|");
         for Row in 1 .. 4 loop
            Ada.Text_IO.Put (" ");
            Real_IO.Put (Matrix.Matrix (Col, Row), 1, 2, 0);
         end loop;
         Ada.Text_IO.Put_Line (" |");
      end loop;
   end Log_Matrix;

   ---------------
   -- Log_State --
   ---------------

   procedure Log_State
     (Name     : String;
      Position : Vector_3;
      Rotation : Quaternion;
      Scale    : Vector_3;
      Matrix   : Matrix_4)
   is
      pragma Unreferenced (Scale);
   begin
      Ada.Text_IO.Put_Line (Name);
      Ada.Text_IO.Put ("pos ");
      Put_Vector (Position);
      Ada.Text_IO.Put ("; quat ");
      Put_Vector (Vector_4'(Vector => Rotation.Vector));
      Ada.Text_IO.New_Line;
      Log_Matrix ("", Matrix);
   end Log_State;

   ------------------------
   -- Log_Transformation --
   ------------------------

   procedure Log_Transformation
     (Name   : String;
      From   : Vector_4;
      To     : Vector_4)
   is
   begin
      Ada.Text_IO.Put (Name & ": ");
      Put_Vector (From);
      Ada.Text_IO.Put (" -> ");
      Put_Vector (To);
      Ada.Text_IO.New_Line;
   end Log_Transformation;

   ----------------
   -- Put_Number --
   ----------------

   procedure Put_Number (X : Real) is
   begin
      Real_IO.Put (X, 1, 2, 0);
   end Put_Number;

   ----------------
   -- Put_Vector --
   ----------------

   procedure Put_Vector (Vector : Vector_3) is
      First : Boolean := True;
   begin
      Ada.Text_IO.Put ("(");
      for X of Vector.Vector loop
         if First then
            First := False;
         else
            Ada.Text_IO.Put (",");
         end if;
         Real_IO.Put (X, 1, 2, 0);
      end loop;
      Ada.Text_IO.Put (")");
   end Put_Vector;

   ----------------
   -- Put_Vector --
   ----------------

   procedure Put_Vector (Vector : Vector_4) is
      First : Boolean := True;
   begin
      Ada.Text_IO.Put ("(");
      for X of Vector.Vector loop
         if First then
            First := False;
         else
            Ada.Text_IO.Put (",");
         end if;
         Real_IO.Put (X, 1, 2, 0);
      end loop;
      Ada.Text_IO.Put (")");
   end Put_Vector;

end Rho.Matrices.Logs;
