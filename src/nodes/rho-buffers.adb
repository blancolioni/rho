package body Rho.Buffers is

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Root_Buffer_Type'Class;
      X      : Real)
   is
   begin
      Buffer.Real_Values.Append (X);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Root_Buffer_Type'Class;
      Vector : Rho.Matrices.Vector_2)
   is
   begin
      Buffer.Real_Values.Append (Rho.Matrices.X (Vector));
      Buffer.Real_Values.Append (Rho.Matrices.Y (Vector));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Root_Buffer_Type'Class;
      Vector :        Rho.Matrices.Vector_3)
   is
   begin
      Buffer.Real_Values.Append (Rho.Matrices.X (Vector));
      Buffer.Real_Values.Append (Rho.Matrices.Y (Vector));
      Buffer.Real_Values.Append (Rho.Matrices.Z (Vector));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Root_Buffer_Type'Class;
      Vector :        Rho.Matrices.Vector_4)
   is
   begin
      Buffer.Real_Values.Append (Rho.Matrices.X (Vector));
      Buffer.Real_Values.Append (Rho.Matrices.Y (Vector));
      Buffer.Real_Values.Append (Rho.Matrices.Z (Vector));
      Buffer.Real_Values.Append (Rho.Matrices.W (Vector));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Root_Buffer_Type'Class;
      Value  : Integer)
   is
   begin
      Buffer.Integer_Values.Append (Value);
   end Append;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Buffer  : Root_Buffer_Type'Class;
      Process : not null access
        procedure (X : Real))
   is
   begin
      for X of Buffer.Real_Values loop
         Process (X);
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Buffer  : Root_Buffer_Type'Class;
      Process : not null access
        procedure (X : Integer))
   is
   begin
      for X of Buffer.Integer_Values loop
         Process (X);
      end loop;
   end Iterate;

end Rho.Buffers;
