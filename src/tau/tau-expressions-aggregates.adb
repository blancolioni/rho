package body Tau.Expressions.Aggregates is

   type Aggregate_Expression_Type is
     new Root_Tau_Expression with
      record
         Aggregate_Type : Tau.Types.Tau_Type;
         Values         : Expression_Array_Holders.Holder;
      end record;

   overriding procedure Check_Names
     (Expression  : Aggregate_Expression_Type;
      Environment : Tau.Environment.Tau_Environment);

   overriding procedure Check_Types
     (Expression    : in out Aggregate_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type);

   overriding function To_String
     (Expression : Aggregate_Expression_Type;
      Generator  : Tau.Generators.Root_Tau_Generator'Class)
      return String;

   ---------------
   -- Aggregate --
   ---------------

   function Aggregate
     (Position       : GCS.Positions.File_Position;
      Aggregate_Type : Tau.Types.Tau_Type;
      Values         : Tau_Expression_Array)
      return Tau_Expression
   is
      Expression : Aggregate_Expression_Type :=
        Aggregate_Expression_Type'
          (Root_Tau_Expression with
           Aggregate_Type => Aggregate_Type,
           Values         => Expression_Array_Holders.To_Holder (Values));
   begin
      Expression.Initialize_Node (Position);
      return new Aggregate_Expression_Type'(Expression);
   end Aggregate;

   -----------------
   -- Check_Names --
   -----------------

   overriding procedure Check_Names
     (Expression  : Aggregate_Expression_Type;
      Environment : Tau.Environment.Tau_Environment)
   is
      Values : constant Tau_Expression_Array := Expression.Values.Element;
   begin
      for Value of Values loop
         Value.Check_Names (Environment);
      end loop;
   end Check_Names;

   -----------------
   -- Check_Types --
   -----------------

   overriding procedure Check_Types
     (Expression    : in out Aggregate_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type)
   is
      Values : constant Tau_Expression_Array := Expression.Values.Element;
      Types  : Tau.Types.Tau_Type_Array (Values'Range);
   begin
      for I in Values'Range loop
         Values (I).Check_Types (Environment, Tau.Types.Any_Type, Types (I));
      end loop;

      if not Environment.Has_Errors then
         if not Expression.Aggregate_Type
           .Is_Convertible_From_Vector (Types)
         then
            Environment.Error (Expression.Defined_At,
                               "invalid values for type "
                               & Expression.Aggregate_Type.Name);
         end if;
      end if;

      if not Expected_Type
        .Is_Convertible_From (Expression.Aggregate_Type)
      then
         Environment.Error (Expression.Defined_At,
                            "expected type " & Expected_Type.Name
                            & " but found "
                            & Expression.Aggregate_Type.Name);
         Found_Type := Expected_Type;
      else
         Found_Type := Expression.Aggregate_Type;
      end if;
   end Check_Types;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression : Aggregate_Expression_Type;
      Generator  : Tau.Generators.Root_Tau_Generator'Class)
      return String
   is
      function Image (Agg : Tau_Expression_Array) return String
      is (if Agg'Length = 0
          then ")"
          elsif Agg'Length = 1
          then Agg (Agg'First).To_String (Generator) & ")"
          else Agg (Agg'First).To_String (Generator) & ","
          & Image (Agg (Agg'First + 1 .. Agg'Last)));

   begin
      return Generator.Shader_Type_Name (Expression.Aggregate_Type.Name)
        & "(" & Image (Expression.Values.Element);
   end To_String;

end Tau.Expressions.Aggregates;
