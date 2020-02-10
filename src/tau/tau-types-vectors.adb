with Tau.Types.Scalar;

package body Tau.Types.Vectors is

   type Tau_Vector_Type is
     new Root_Tau_Type with
      record
         Order : Positive;
      end record;

   overriding function Is_Convertible_From_Vector
     (Vector : not null access Tau_Vector_Type;
      From   : Tau_Type_Array)
      return Boolean;

   Local_Vectors : array (2 .. 4) of Tau_Type;
   Have_Local_Vectors : Boolean := False;

   procedure Check_Local_Vectors;

   -------------------------
   -- Check_Local_Vectors --
   -------------------------

   procedure Check_Local_Vectors is

      Axes : constant String := "xyzw";

      procedure Create_Properties
        (V : in out Tau_Vector_Type);

      -----------------------
      -- Create_Properties --
      -----------------------

      procedure Create_Properties
        (V : in out Tau_Vector_Type)
      is
         Ps : constant String := Axes (1 .. V.Order);
      begin
         for Ch of Ps loop
            V.Properties.Insert
              (Key      => (1 => Ch),
               New_Item => Tau.Types.Scalar.Tau_Float);
         end loop;
      end Create_Properties;

   begin
      if not Have_Local_Vectors then
         for I in Local_Vectors'Range loop
            declare
               V : Tau_Vector_Type :=
                 Tau_Vector_Type'
                   (Root_Tau_Type with
                    Order => I);
            begin
               V.Initialize_Object
                 (GCS.Positions.Null_Position,
                  "vector_" & Character'Val (Character'Pos ('0') + I));
               Create_Properties (V);
               Local_Vectors (I) := new Tau_Vector_Type'(V);
            end;
         end loop;
         Have_Local_Vectors := True;
      end if;
   end Check_Local_Vectors;

   --------------------------------
   -- Is_Convertible_From_Vector --
   --------------------------------

   overriding function Is_Convertible_From_Vector
     (Vector : not null access Tau_Vector_Type;
      From   : Tau_Type_Array)
      return Boolean
   is
   begin
      return From'Length = Vector.Order;
   end Is_Convertible_From_Vector;

   ------------
   -- Vector --
   ------------

   function Vector (Order : Positive) return Tau_Type is
   begin
      Check_Local_Vectors;
      return Local_Vectors (Order);
   end Vector;

end Tau.Types.Vectors;
