private with Ada.Containers.Indefinite_Holders;

with Tau.Environment;
with Tau.Generators;
with Tau.Types;

package Tau.Expressions is

   type Root_Tau_Expression is
     abstract new Root_Tau_Node with private;

   procedure Check
     (Expression    : in out Root_Tau_Expression;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type);

   procedure Check_Names
     (Expression  : in out Root_Tau_Expression;
      Environment : Tau.Environment.Tau_Environment)
   is abstract;

   procedure Check_Types
     (Expression    : in out Root_Tau_Expression;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type)
   is abstract;

   function To_String
     (Expression : Root_Tau_Expression;
      Generator  : in out Tau.Generators.Root_Tau_Generator'Class)
      return String
      is abstract;

   type Tau_Expression is access all Root_Tau_Expression'Class;

   function Literal
     (Position : GCS.Positions.File_Position;
      Value    : Float)
      return Tau_Expression;

   function Literal
     (Position : GCS.Positions.File_Position;
      Value    : Integer)
      return Tau_Expression;

   function Reference
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Tau_Expression;

   function Property
     (Position  : GCS.Positions.File_Position;
      Left      : Tau_Expression;
      Name      : String)
      return Tau_Expression;

   function Add (Left, Right : Tau_Expression) return Tau_Expression;
   function Subtract (Left, Right : Tau_Expression) return Tau_Expression;
   function Multiply (Left, Right : Tau_Expression) return Tau_Expression;
   function Divide (Left, Right : Tau_Expression) return Tau_Expression;

   type Tau_Expression_Array is array (Positive range <>) of Tau_Expression;

   function Aggregate
     (Position    : GCS.Positions.File_Position;
      Return_Type : Tau.Types.Tau_Type;
      Values      : Tau_Expression_Array)
      return Tau_Expression;

   function Apply
     (Position      : GCS.Positions.File_Position;
      Function_Name : String;
      Values        : Tau_Expression_Array)
      return Tau_Expression;

private

   package Expression_Array_Holders is
     new Ada.Containers.Indefinite_Holders (Tau_Expression_Array);

   type Root_Tau_Expression is abstract new Root_Tau_Node with
      record
         null;
      end record;

   overriding function Class_Name
     (Item : Root_Tau_Expression)
      return String
   is ("expression");

end Tau.Expressions;
