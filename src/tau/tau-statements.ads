with Tau.Environment;
with Tau.Expressions;
with Tau.Generators;

package Tau.Statements is

   type Root_Tau_Statement is
     abstract new Root_Tau_Node with private;

   procedure Check
     (Statement     : in out Root_Tau_Statement;
      Environment   : Tau.Environment.Tau_Environment)
   is abstract;

   procedure Compile
     (Statement : Root_Tau_Statement;
      Generator : in out Tau.Generators.Root_Tau_Generator'Class)
   is abstract;

   function Return_Value
     (Statement : Root_Tau_Statement)
      return Tau.Expressions.Tau_Expression;

   function Assigned_Value
     (Statement : Root_Tau_Statement'Class)
      return Tau.Expressions.Tau_Expression;

   function Target
     (Statement : Root_Tau_Statement'Class)
      return String;

   type Tau_Statement is access all Root_Tau_Statement'Class;

   function Return_Statement
     (Position : GCS.Positions.File_Position;
      Value    : Tau.Expressions.Tau_Expression)
      return Tau_Statement;

   function Assignment_Statement
     (Position : GCS.Positions.File_Position;
      Name     : String;
      Value    : Tau.Expressions.Tau_Expression)
      return Tau_Statement;

private

   type Root_Tau_Statement is
     abstract new Root_Tau_Node with
      record
         null;
      end record;

   overriding function Class_Name
     (Item : Root_Tau_Statement)
      return String
   is ("statement");

end Tau.Statements;
