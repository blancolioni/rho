private package Tau.Expressions.Operators is

   type Infix_Operator_Type is
     (Op_Add,
      Op_Subtract,
      Op_Multiply,
      Op_Divide,
      Op_Power,
      Op_Equal,
      Op_Not_Equal);

   type Prefix_Operator_Type is
     (Op_Plus, Op_Minus);

   function Operate
     (Operator    : Infix_Operator_Type;
      Left, Right : Tau_Expression)
      return Tau_Expression;

end Tau.Expressions.Operators;
