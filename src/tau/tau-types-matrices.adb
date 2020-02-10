package body Tau.Types.Matrices is

   type Tau_Matrix_Type is
     new Root_Tau_Type with
      record
         Row_Order : Positive;
         Col_Order : Positive;
      end record;

   Local_Matrices      : array (1 .. 4, 1 .. 4) of Tau_Type;
   Have_Local_Matrices : Boolean := False;

   procedure Check_Local_Matrices;

   -------------------------
   -- Check_Local_Matrices --
   -------------------------

   procedure Check_Local_Matrices is

      function Matrix_Type_Name (Rows, Cols : Positive) return String;

      ----------------------
      -- Matrix_Type_Name --
      ----------------------

      function Matrix_Type_Name (Rows, Cols : Positive) return String is
      begin
         if Rows = Cols then
            return "matrix_" & Character'Val (Character'Pos ('0') + Rows);
         else
            return "matrix_"
              & Character'Val (Character'Pos ('0') + Rows)
              & "x"
              & Character'Val (Character'Pos ('0') + Cols);
         end if;
      end Matrix_Type_Name;

   begin
      if not Have_Local_Matrices then
         for Row_Order in Local_Matrices'Range (1) loop
            for Col_Order in Local_Matrices'Range (2) loop
               declare
                  M : Tau_Matrix_Type :=
                    Tau_Matrix_Type'
                      (Root_Tau_Type with
                       Row_Order => Row_Order,
                       Col_Order => Col_Order);
               begin
                  M.Initialize_Object
                    (GCS.Positions.Null_Position,
                     Matrix_Type_Name (Row_Order, Col_Order));
                  Local_Matrices (Row_Order, Col_Order) :=
                    new Tau_Matrix_Type'(M);
               end;
            end loop;
         end loop;
         Have_Local_Matrices := True;
      end if;
   end Check_Local_Matrices;

   ------------
   -- Matrix --
   ------------

   function Matrix (Row_Order, Column_Order : Positive) return Tau_Type is
   begin
      Check_Local_Matrices;
      return Local_Matrices (Row_Order, Column_Order);
   end Matrix;

end Tau.Types.Matrices;
