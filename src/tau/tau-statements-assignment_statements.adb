with Ada.Strings.Unbounded;

package body Tau.Statements.Assignment_Statements is

   type Assignment_Statement_Type is
     new Root_Tau_Statement with
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Value : Tau.Expressions.Tau_Expression;
      end record;

   overriding procedure Check
     (Statement     : Assignment_Statement_Type;
      Environment   : Tau.Environment.Tau_Environment);

   overriding procedure Compile
     (Statement : Assignment_Statement_Type;
      Generator : in out Tau.Generators.Root_Tau_Generator'Class);

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Statement     : Assignment_Statement_Type;
      Environment   : Tau.Environment.Tau_Environment)
   is
      Name : constant String :=
        Ada.Strings.Unbounded.To_String (Statement.Name);
   begin
      if not Environment.Contains (Name) then
         Environment.Error (Statement.Defined_At,
                            "undefined: " & Name);
      else
         Statement.Value.Check
           (Environment,
            Environment.Get (Name).Entry_Type);
      end if;
   end Check;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Statement : Assignment_Statement_Type;
      Generator : in out Tau.Generators.Root_Tau_Generator'Class)
   is
   begin
      Generator.Set_Value
        (To_Name => Ada.Strings.Unbounded.To_String (Statement.Name),
         Value   => Statement.Value.To_String (Generator));
   end Compile;

   ------------
   -- Create --
   ------------

   function Create
     (Position : GCS.Positions.File_Position;
      Target   : String;
      Value    : Tau.Expressions.Tau_Expression)
      return Tau_Statement
   is
      Statement : Assignment_Statement_Type := Assignment_Statement_Type'
        (Root_Tau_Node with
         Name  => Ada.Strings.Unbounded.To_Unbounded_String (Target),
         Value => Value);
   begin
      Statement.Initialize_Node (Position);
      return new Assignment_Statement_Type'(Statement);
   end Create;

end Tau.Statements.Assignment_Statements;
