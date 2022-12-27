private with Ada.Containers.Vectors;

with Rho.Objects;
with Rho.Matrices;
with Rho.Shaders.Variables;

package Rho.Buffers is

   type Buffer_Contents_Type is
     (Integer_Data,
      Real_Data,
      Vector_2_Data,
      Vector_3_Data,
      Vector_4_Data);

   type Root_Buffer_Type is new Rho.Objects.Root_Object_Type with private;

   function Contents (Buffer : Root_Buffer_Type) return Buffer_Contents_Type;

   procedure Append
     (Buffer : in out Root_Buffer_Type'Class;
      X      : Real)
     with Pre => Buffer.Contents = Real_Data;

   procedure Append
     (Buffer : in out Root_Buffer_Type'Class;
      Vector : Rho.Matrices.Vector_2)
     with Pre => Buffer.Contents = Vector_2_Data;

   procedure Append
     (Buffer : in out Root_Buffer_Type'Class;
      Vector : Rho.Matrices.Vector_3)
     with Pre => Buffer.Contents = Vector_3_Data;

   procedure Append
     (Buffer : in out Root_Buffer_Type'Class;
      Vector : Rho.Matrices.Vector_4)
     with Pre => Buffer.Contents = Vector_4_Data;

   procedure Append
     (Buffer : in out Root_Buffer_Type'Class;
      Value  : Integer)
     with Pre => Buffer.Contents = Integer_Data;

   function Element_Count (Buffer : Root_Buffer_Type'Class) return Natural;
   function Scalar_Count (Buffer : Root_Buffer_Type'Class) return Natural;

   type Buffer_Type is access all Root_Buffer_Type'Class;

   function Create_Buffer (Contents : Buffer_Contents_Type) return Buffer_Type;

   type Buffer_Handler_Interface is limited interface;

   procedure Load_Buffer
     (Handler : in out Buffer_Handler_Interface;
      Buffer  : Buffer_Type)
   is abstract;

   procedure Activate_Buffer
     (Handler  : in out Buffer_Handler_Interface;
      Buffer   : Buffer_Type;
      Argument : access Rho.Shaders.Variables.Root_Variable_Type'Class)
   is abstract;

   procedure Iterate
     (Buffer : Root_Buffer_Type'Class;
      Process : not null access
        procedure (X : Real))
       with Pre => Buffer.Contents /= Integer_Data;

   procedure Iterate
     (Buffer  : Root_Buffer_Type'Class;
      Process : not null access
        procedure (X : Integer))
       with Pre => Buffer.Contents = Integer_Data;

private

   package Real_Vectors is
     new Ada.Containers.Vectors (Positive, Real);

   package Integer_Vectors is
     new Ada.Containers.Vectors (Positive, Integer);

   type Root_Buffer_Type is new Rho.Objects.Root_Object_Type with
      record
         Contents       : Buffer_Contents_Type;
         Real_Values    : Real_Vectors.Vector;
         Integer_Values : Integer_Vectors.Vector;
      end record;

   overriding function Class_Name
     (Buffer : Root_Buffer_Type)
      return String
   is ("buffer");

   function Contents (Buffer : Root_Buffer_Type) return Buffer_Contents_Type
   is (Buffer.Contents);

   function Create_Buffer (Contents : Buffer_Contents_Type) return Buffer_Type
   is (new Root_Buffer_Type'(Rho.Objects.Root_Object_Type with
                               Contents => Contents, others => <>));

   function Element_Count (Buffer : Root_Buffer_Type'Class) return Natural
   is (case Buffer.Contents is
          when Integer_Data => Buffer.Integer_Values.Last_Index,
          when Real_Data    => Buffer.Real_Values.Last_Index,
          when Vector_2_Data => Buffer.Real_Values.Last_Index / 3,
          when Vector_3_Data => Buffer.Real_Values.Last_Index / 3,
          when Vector_4_Data => Buffer.Real_Values.Last_Index / 4);

   function Scalar_Count (Buffer : Root_Buffer_Type'Class) return Natural
   is (case Buffer.Contents is
          when Integer_Data  => Buffer.Integer_Values.Last_Index,
          when Real_Data
            | Vector_2_Data
              | Vector_3_Data
                | Vector_4_Data    => Buffer.Real_Values.Last_Index);

end Rho.Buffers;
