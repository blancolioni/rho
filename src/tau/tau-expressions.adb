with Tau.Expressions.Aggregates;
with Tau.Expressions.Conditional;
with Tau.Expressions.Functions;
with Tau.Expressions.Literals;
with Tau.Expressions.Operators;
with Tau.Expressions.Properties;
with Tau.Expressions.References;

package body Tau.Expressions is

   ---------
   -- Add --
   ---------

   function Add (Left, Right : Tau_Expression) return Tau_Expression is
   begin
      return Operators.Operate
        (Operator => Operators.Op_Add,
         Left     => Left,
         Right    => Right);
   end Add;

   ---------------
   -- Aggregate --
   ---------------

   function Aggregate
     (Position    : GCS.Positions.File_Position;
      Return_Type : Tau.Types.Tau_Type;
      Values      : Tau_Expression_Array)
      return Tau_Expression
   is
   begin
      return Tau.Expressions.Aggregates.Aggregate
        (Position, Return_Type, Values);
   end Aggregate;

   -----------
   -- Apply --
   -----------

   function Apply
     (Position      : GCS.Positions.File_Position;
      Function_Name : String;
      Values        : Tau_Expression_Array)
      return Tau_Expression
   is
   begin
      return Tau.Expressions.Functions.Apply
        (Position, Function_Name, Values);
   end Apply;

   -----------
   -- Check --
   -----------

   procedure Check
     (Expression    : in out Root_Tau_Expression;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type)
   is
      Found_Type : Tau.Types.Tau_Type with Unreferenced;
      Expr_Class : Root_Tau_Expression'Class renames
        Root_Tau_Expression'Class (Expression);
   begin
      Expr_Class.Check_Names (Environment);
      if not Expr_Class.Has_Errors then
         Expr_Class.Check_Types
           (Environment, Expected_Type, Found_Type);
      end if;
   end Check;

   ---------------
   -- Condition --
   ---------------

   function Condition
     (Position       : GCS.Positions.File_Position;
      Condition      : Tau_Expression;
      True_Value     : Tau_Expression;
      False_Value    : Tau_Expression)
      return Tau_Expression
   is
   begin
      return Tau.Expressions.Conditional.Conditional_Expression
        (Position, Condition, True_Value, False_Value);
   end Condition;

   ------------
   -- Divide --
   ------------

   function Divide (Left, Right : Tau_Expression) return Tau_Expression is
   begin
      return Operators.Operate
        (Operator => Operators.Op_Divide,
         Left     => Left,
         Right    => Right);
   end Divide;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Tau_Expression) return Tau_Expression is
   begin
      return Operators.Operate
        (Operator => Operators.Op_Equal,
         Left     => Left,
         Right    => Right);
   end Equal;

   -------------
   -- Literal --
   -------------

   function Literal (Position    : GCS.Positions.File_Position;
                     Value       : Float)
                     return Tau_Expression
   is
   begin
      return Tau.Expressions.Literals.Literal_Expression
        (Position, Value);
   end Literal;

   -------------
   -- Literal --
   -------------

   function Literal (Position    : GCS.Positions.File_Position;
                     Value       : Integer)
                     return Tau_Expression
   is
   begin
      return Tau.Expressions.Literals.Literal_Expression
        (Position, Value);
   end Literal;

   --------------
   -- Multiply --
   --------------

   function Multiply (Left, Right : Tau_Expression) return Tau_Expression is
   begin
      return Operators.Operate
        (Operator => Operators.Op_Multiply,
         Left     => Left,
         Right    => Right);
   end Multiply;

   ---------------
   -- Not_Equal --
   ---------------

   function Not_Equal (Left, Right : Tau_Expression) return Tau_Expression is
   begin
      return Operators.Operate
        (Operator => Operators.Op_Not_Equal,
         Left     => Left,
         Right    => Right);
   end Not_Equal;

   -----------
   -- Power --
   -----------

   function Power (Left, Right : Tau_Expression) return Tau_Expression is
   begin
      return Operators.Operate
        (Operator => Operators.Op_Power,
         Left     => Left,
         Right    => Right);
   end Power;

   --------------
   -- Property --
   --------------

   function Property
     (Position  : GCS.Positions.File_Position;
      Left      : Tau_Expression;
      Name      : String)
      return Tau_Expression
   is
   begin
      return Tau.Expressions.Properties.Property_Expression
        (Position, Left, Name);
   end Property;

   ---------------
   -- Reference --
   ---------------

   function Reference (Position    : GCS.Positions.File_Position;
                       Name        : String)
                       return Tau_Expression
   is
   begin
      return Tau.Expressions.References.Reference_Expression
        (Position, Name);
   end Reference;

   --------------
   -- Subtract --
   --------------

   function Subtract (Left, Right : Tau_Expression) return Tau_Expression is
   begin
      return Operators.Operate
        (Operator => Operators.Op_Subtract,
         Left     => Left,
         Right    => Right);
   end Subtract;

end Tau.Expressions;
