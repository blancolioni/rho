with Ada.Strings.Unbounded;

package body Tau.Statements.Assignment_Statements is

   type Assignment_Statement_Type is
     new Root_Tau_Statement with
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Value : Tau.Expressions.Tau_Expression;
      end record;

   overriding function Children
     (Statement : Assignment_Statement_Type)
      return Tau_Node_Array
   is (Root_Tau_Statement (Statement).Children
       & (1 => Tau_Node (Statement.Value)));

   overriding procedure Check
     (Statement     : in out Assignment_Statement_Type;
      Environment   : Tau.Environment.Tau_Environment);

   overriding procedure Compile
     (Statement : Assignment_Statement_Type;
      Generator : in out Tau.Generators.Root_Tau_Generator'Class);

   overriding function Depends_On
     (Statement : Assignment_Statement_Type;
      Name      : String)
      return Boolean
   is (Name /= -(Statement.Name) and then Statement.Value.Depends_On (Name));

   --------------------
   -- Assigned_Value --
   --------------------

   function Assigned_Value
     (Statement : Root_Tau_Statement'Class)
      return Tau.Expressions.Tau_Expression
   is
   begin
      if Statement in Assignment_Statement_Type'Class then
         return Assignment_Statement_Type'Class (Statement).Value;
      else
         return null;
      end if;
   end Assigned_Value;

   -----------------------
   -- Assignment_Target --
   -----------------------

   function Assignment_Target
     (Statement : Root_Tau_Statement'Class)
      return String
   is
   begin
      if Statement in Assignment_Statement_Type'Class then
         return -Assignment_Statement_Type'Class (Statement).Name;
      else
         return "";
      end if;
   end Assignment_Target;

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Statement     : in out Assignment_Statement_Type;
      Environment   : Tau.Environment.Tau_Environment)
   is
      Name : constant String :=
        Ada.Strings.Unbounded.To_String (Statement.Name);
   begin
      if not Environment.Contains (Name) then
         Statement.Error ("undefined: " & Name);
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
        (Generator.Normalize_Reference (-(Statement.Name)),
         Statement.Value.To_String (Generator));
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
