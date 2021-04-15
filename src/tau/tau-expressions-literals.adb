with Tau.Types.Scalar;

package body Tau.Expressions.Literals is

   type Float_Literal_Type is
     new Root_Literal_Expression with
      record
         Float_Value : Float;
      end record;

   overriding procedure Check_Names
     (Expression  : in out Float_Literal_Type;
      Environment : Tau.Environment.Tau_Environment)
   is null;

   overriding procedure Check_Types
     (Expression    : in out Float_Literal_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type);

   overriding function To_String
     (Expression : Float_Literal_Type;
      Generator  : Tau.Generators.Root_Tau_Generator'Class)
      return String
   is (Generator.Float_Image (Expression.Float_Value));

   type Integer_Literal_Type is
     new Root_Tau_Expression with
      record
         Integer_Value : Integer;
      end record;

   overriding procedure Check_Names
     (Expression  : in out Integer_Literal_Type;
      Environment : Tau.Environment.Tau_Environment)
   is null;

   overriding procedure Check_Types
     (Expression    : in out Integer_Literal_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type);

   overriding function To_String
     (Expression : Integer_Literal_Type;
      Generator  : Tau.Generators.Root_Tau_Generator'Class)
      return String
   is (Generator.Integer_Image (Expression.Integer_Value));

   -----------------
   -- Check_Types --
   -----------------

   overriding procedure Check_Types
     (Expression    : in out Float_Literal_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type)
   is
      pragma Unreferenced (Environment);
   begin
      if not Expected_Type.Is_Convertible_From
        (Tau.Types.Scalar.Tau_Float)
      then
         Expression.Error
           ("expected " & Expected_Type.Name
            & " but found a floating point number");
      end if;
      Found_Type := Expected_Type;
   end Check_Types;

   -----------------
   -- Check_Types --
   -----------------

   overriding procedure Check_Types
     (Expression    : in out Integer_Literal_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type)
   is
      pragma Unreferenced (Environment);
   begin
      if not Expected_Type.Is_Convertible_From
        (Tau.Types.Scalar.Tau_Integer)
      then
         Expression.Error
           ("expected " & Expected_Type.Name
            & " but found an integer");
      end if;
      Found_Type := Expected_Type;
   end Check_Types;

   ------------------------
   -- Literal_Expression --
   ------------------------

   function Literal_Expression
     (Position : GCS.Positions.File_Position;
      Value    : Float)
      return Tau_Expression
   is
      Expression : Float_Literal_Type;
   begin
      Expression.Initialize_Node (Position);
      Expression.Float_Value := Value;
      return new Float_Literal_Type'(Expression);
   end Literal_Expression;

   ------------------------
   -- Literal_Expression --
   ------------------------

   function Literal_Expression
     (Position : GCS.Positions.File_Position;
      Value    : Integer)
      return Tau_Expression
   is
      Expression : Integer_Literal_Type;
   begin
      Expression.Initialize_Node (Position);
      Expression.Integer_Value := Value;
      return new Integer_Literal_Type'(Expression);
   end Literal_Expression;

end Tau.Expressions.Literals;
