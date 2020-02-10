package body Tau.Statements.Return_Statements is

   type Return_Statement_Type is
     new Root_Tau_Statement with
      record
         Value : Tau.Expressions.Tau_Expression;
      end record;

   overriding procedure Check
     (Statement     : Return_Statement_Type;
      Environment   : Tau.Environment.Tau_Environment);

   overriding procedure Compile
     (Statement : Return_Statement_Type;
      Generator : in out Tau.Generators.Root_Tau_Generator'Class);

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Statement     : Return_Statement_Type;
      Environment   : Tau.Environment.Tau_Environment)
   is
   begin
      if not Environment.Has_Return_Type then
         Environment.Error
           (Statement,
            "function cannot return a value");
      else
         Statement.Value.Check (Environment, Environment.Return_Type);
      end if;

   end Check;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Statement : Return_Statement_Type;
      Generator : in out Tau.Generators.Root_Tau_Generator'Class)
   is
   begin
      Generator.Return_Value (Statement.Value.To_String (Generator));
   end Compile;

   ------------
   -- Create --
   ------------

   function Create
     (Position : GCS.Positions.File_Position;
      Value    : Tau.Expressions.Tau_Expression)
      return Tau_Statement
   is
      Statement : Return_Statement_Type := Return_Statement_Type'
        (Root_Tau_Node with
         Value       => Value);
   begin
      Statement.Initialize_Node (Position);
      return new Return_Statement_Type'(Statement);
   end Create;

end Tau.Statements.Return_Statements;
