with Tau.Types.Standard;

package body Tau.Expressions.Conditional is

   type Conditional_Expression_Type is
     new Root_Tau_Expression with
      record
         Condition   : Tau_Expression;
         True_Value  : Tau_Expression;
         False_Value : Tau_Expression;
      end record;

   overriding function Children
     (Expression : Conditional_Expression_Type)
      return Tau_Node_Array;

   overriding procedure Check_Names
     (Expression  : in out Conditional_Expression_Type;
      Environment : Tau.Environment.Tau_Environment);

   overriding procedure Check_Types
     (Expression    : in out Conditional_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type);

   overriding function To_String
     (Expression : Conditional_Expression_Type;
      Generator  : Tau.Generators.Root_Tau_Generator'Class)
      return String;

   -----------------
   -- Check_Names --
   -----------------

   overriding procedure Check_Names
     (Expression  : in out Conditional_Expression_Type;
      Environment : Tau.Environment.Tau_Environment)
   is
   begin
      Expression.Condition.Check_Names (Environment);
      Expression.True_Value.Check_Names (Environment);
      Expression.False_Value.Check_Names (Environment);
   end Check_Names;

   -----------------
   -- Check_Types --
   -----------------

   overriding procedure Check_Types
     (Expression    : in out Conditional_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type)
   is
      Found_Condition_Type : Tau.Types.Tau_Type;
      True_Value_Type      : Tau.Types.Tau_Type;
      False_Value_Type     : Tau.Types.Tau_Type;
   begin
      Expression.Condition.Check_Types
        (Environment, Tau.Types.Standard.Tau_Boolean, Found_Condition_Type);
      Expression.True_Value.Check_Types
        (Environment, Expected_Type, True_Value_Type);
      Expression.False_Value.Check_Types
        (Environment, Expected_Type, False_Value_Type);

      if not Tau.Types.Standard.Tau_Boolean.Is_Convertible_From
        (Found_Condition_Type)
      then
         Expression.Condition.Error
           ("expected boolean type"
            & " but found '" & True_Value_Type.Name & "'");
      end if;

      if not Expected_Type.Is_Convertible_From (True_Value_Type) then
         Expression.True_Value.Error
           ("expected type '" & Expected_Type.Name & "'"
            & " but found '" & True_Value_Type.Name & "'");
      end if;

      if not Expected_Type.Is_Convertible_From (False_Value_Type) then
         Expression.False_Value.Error
           ("expected type '" & Expected_Type.Name & "'"
            & " but found '" & False_Value_Type.Name & "'");
      end if;

      Found_Type := True_Value_Type;

   end Check_Types;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Expression : Conditional_Expression_Type)
      return Tau_Node_Array
   is
      Base_Children : constant Tau_Node_Array :=
        Root_Tau_Expression (Expression).Children;
   begin
      return Base_Children
        & (Tau_Node (Expression.Condition),
           Tau_Node (Expression.True_Value),
           Tau_Node (Expression.False_Value));
   end Children;

   ----------------------------
   -- Conditional_Expression --
   ----------------------------

   function Conditional_Expression
     (Position       : GCS.Positions.File_Position;
      Condition      : Tau_Expression;
      True_Value     : Tau_Expression;
      False_Value    : Tau_Expression)
      return Tau_Expression
   is
      Expression : Conditional_Expression_Type :=
                     Conditional_Expression_Type'
                       (Root_Tau_Expression with
                        Condition     => Condition,
                        True_Value    => True_Value,
                        False_Value   => False_Value);
   begin
      Expression.Initialize_Node (Position);
      return new Conditional_Expression_Type'(Expression);
   end Conditional_Expression;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression : Conditional_Expression_Type;
      Generator  : Tau.Generators.Root_Tau_Generator'Class)
      return String
   is
   begin
      return "(" & Expression.Condition.To_String (Generator) & ")"
        & " ? "
        & "(" & Expression.True_Value.To_String (Generator) & ")"
        & " : "
        & "(" & Expression.False_Value.To_String (Generator) & ")";
   end To_String;

end Tau.Expressions.Conditional;
