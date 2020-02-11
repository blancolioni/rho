with Ada.Containers.Doubly_Linked_Lists;

with Tau.Types.Standard;

package body Tau.Expressions.Operators is

   type Infix_Signature_Type is
      record
         Left, Right : Tau.Types.Tau_Type;
         Result      : Tau.Types.Tau_Type;
      end record;

   package Infix_Signature_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Infix_Signature_Type);

   package Type_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Tau.Types.Tau_Type, Tau.Types."=");

   type Possible_Infix_Type is
      record
         List : Infix_Signature_Lists.List;
      end record;

   Infix_Ops : array (Infix_Operator_Type) of Possible_Infix_Type;
   Have_Ops  : Boolean := False;

   function Operator_Name
     (Operator : Infix_Operator_Type)
      return String
   is (case Operator is
          when Op_Add => "+",
          when Op_Divide => "/",
          when Op_Multiply => "*",
          when Op_Subtract => "-");

   procedure Check_Operator_Table;

   type Infix_Expression_Type is
     new Root_Tau_Expression with
      record
         Operator    : Infix_Operator_Type;
         Left, Right : Tau_Expression;
      end record;

   procedure Infer_Types
     (Expression  : Infix_Expression_Type'Class;
      Environment : Tau.Environment.Tau_Environment;
      Expected    : Type_Lists.List;
      Found       : in out Type_Lists.List;
      Depth       : Natural := 0);

   overriding function Children
     (Expression : Infix_Expression_Type)
      return Tau_Node_Array
   is (Root_Tau_Expression (Expression).Children
       & (Tau_Node (Expression.Left), Tau_Node (Expression.Right)));

   overriding procedure Check_Names
     (Expression    : in out Infix_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment);

   overriding procedure Check_Types
     (Expression    : in out Infix_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type);

   overriding function To_String
     (Expression : Infix_Expression_Type;
      Generator  : in out Tau.Generators.Root_Tau_Generator'Class)
      return String
   is ("("
       & Expression.Left.To_String (Generator)
       & " "
       & Operator_Name (Expression.Operator)
       & " "
       & Expression.Right.To_String (Generator)
       & ")");

   -----------------
   -- Check_Names --
   -----------------

   overriding procedure Check_Names
     (Expression    : in out Infix_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment)
   is
   begin
      Expression.Left.Check_Names (Environment);
      Expression.Right.Check_Names (Environment);
   end Check_Names;

   --------------------------
   -- Check_Operator_Table --
   --------------------------

   procedure Check_Operator_Table is

      procedure Add
        (Op   : Infix_Operator_Type;
         List : in out Infix_Signature_Lists.List);

      ---------
      -- Add --
      ---------

      procedure Add
        (Op   : Infix_Operator_Type;
         List : in out Infix_Signature_Lists.List)
      is
         I : constant Tau.Types.Tau_Type :=
           Tau.Types.Standard.Tau_Integer;
         F : constant Tau.Types.Tau_Type :=
           Tau.Types.Standard.Tau_Float;

         procedure Add (Left, Right, Result : Tau.Types.Tau_Type);

         ---------
         -- Add --
         ---------

         procedure Add (Left, Right, Result : Tau.Types.Tau_Type) is
         begin
            List.Append ((Left, Right, Result));
         end Add;

      begin
         Add (I, I, I);
         Add (F, F, F);

         for I in 2 .. 4 loop
            declare
               M : constant Tau.Types.Tau_Type :=
                 Tau.Types.Standard.Tau_Matrix (I, I);
               V : constant Tau.Types.Tau_Type :=
                 Tau.Types.Standard.Tau_Vector (I);
            begin
               Add (M, M, M);

               if Op in Op_Multiply | Op_Divide then
                  Add (M, F, M);
                  Add (V, F, M);
                  Add (F, M, M);
                  Add (F, V, V);
               end if;

               if Op = Op_Multiply then
                  Add (M, V, V);
               elsif Op /= Op_Divide then
                  Add (V, V, V);
               end if;
            end;
         end loop;
      end Add;

   begin
      if not Have_Ops then
         for Op in Infix_Ops'Range loop
            Add (Op, Infix_Ops (Op).List);
         end loop;
         Have_Ops := True;
      end if;
   end Check_Operator_Table;

   -----------------
   -- Check_Types --
   -----------------

   overriding procedure Check_Types
     (Expression    : in out Infix_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type)
   is
      Expected : Type_Lists.List;
      Possible : Type_Lists.List;
   begin
      Check_Operator_Table;

      Expected.Append (Expected_Type);
      Infer_Types (Infix_Expression_Type'Class (Expression),
                   Environment, Expected, Possible);

      if Possible.Is_Empty then
         Expression.Error
           ("invalid types for operator "
            & Operator_Name (Expression.Operator));
         Found_Type := Expected_Type;
      else
--           Rho.Logging.Log
--             ("infer: " & Expression.Operator'Image
--              & ": result type " & Possible.First_Element.Name);

         pragma Assert
           (Expected_Type.Is_Convertible_From (Possible.First_Element));
         Found_Type := Possible.First_Element;
      end if;

   end Check_Types;

   -----------------
   -- Infer_Types --
   -----------------

   procedure Infer_Types
     (Expression  : Infix_Expression_Type'Class;
      Environment : Tau.Environment.Tau_Environment;
      Expected    : Type_Lists.List;
      Found       : in out Type_Lists.List;
      Depth       : Natural := 0)
   is

--        Indent : constant String (1 .. Depth) := (others => ' ');

      Matches : Possible_Infix_Type;

      procedure Infer_Child
        (Child   : Tau_Expression;
         Expect  : Tau.Types.Tau_Type;
         Matches : out Type_Lists.List);

      -----------------
      -- Infer_Child --
      -----------------

      procedure Infer_Child
        (Child   : Tau_Expression;
         Expect  : Tau.Types.Tau_Type;
         Matches : out Type_Lists.List)
      is
      begin
         if Child.all in Infix_Expression_Type'Class then
            declare
               Infix : Infix_Expression_Type renames
                 Infix_Expression_Type (Child.all);
               Expected : Type_Lists.List;
            begin
               Expected.Append (Expect);
               Infix.Infer_Types
                 (Environment => Environment,
                  Expected    => Expected,
                  Found       => Matches,
                  Depth       => Depth + 2);
            end;
         else
            declare
               Found : Tau.Types.Tau_Type;
            begin
               Child.Check_Types (Environment, Expect, Found);
               if not Child.Has_Errors then
                  Matches.Append (Found);
               end if;
               Child.Clear_Errors;
            end;
         end if;
      end Infer_Child;

   begin

      for Signature of Infix_Ops (Expression.Operator) .List loop
         for Expect of Expected loop

            if Expect.Is_Convertible_From (Signature.Result) then
--                 Ada.Text_IO.Put_Line
--                   (Indent & "infer: " & Signature.Left.Name
--                    & " x " & Signature.Right.Name
--                    & " -> "
--                    & Signature.Result.Name);
               declare
                  Left_Expect  : Type_Lists.List;
                  Right_Expect : Type_Lists.List;
                  Left_Matches  : Type_Lists.List;
                  Right_Matches : Type_Lists.List;
               begin
                  Left_Expect.Append (Signature.Left);
                  Right_Expect.Append (Signature.Right);
                  Infer_Child
                    (Expression.Left, Signature.Left, Left_Matches);
                  Infer_Child
                    (Expression.Right, Signature.Right, Right_Matches);
                  if not Left_Matches.Is_Empty
                    and then not Right_Matches.Is_Empty
                  then
                     Matches.List.Append
                       ((Signature.Left, Signature.Right, Expect));
                     Found.Append (Expect);
                  end if;
               end;
            end if;
         end loop;
      end loop;
   end Infer_Types;

   -------------
   -- Operate --
   -------------

   function Operate
     (Operator    : Infix_Operator_Type;
      Left, Right : Tau_Expression)
      return Tau_Expression
   is
      Expression : Infix_Expression_Type :=
        Infix_Expression_Type'
          (Root_Tau_Expression with
           Operator    => Operator,
           Left        => Left,
           Right       => Right);
   begin
      Expression.Initialize_Node (Left.Defined_At);
      return new Infix_Expression_Type'(Expression);
   end Operate;

end Tau.Expressions.Operators;
