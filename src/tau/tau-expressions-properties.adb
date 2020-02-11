with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Tau.Entries;

package body Tau.Expressions.Properties is

   type Property_Expression_Type is
     new Root_Tau_Expression with
      record
         Value    : Tau.Expressions.Tau_Expression;
         Property : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Children
     (Expression : Property_Expression_Type)
      return Tau_Node_Array
   is (Root_Tau_Expression (Expression).Children
       & Tau_Node (Expression.Value));

   overriding procedure Check_Names
     (Expression    : in out Property_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment);

   overriding procedure Check_Types
     (Expression    : in out Property_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type);

   overriding function To_String
     (Expression : Property_Expression_Type;
      Generator  : in out Tau.Generators.Root_Tau_Generator'Class)
      return String;

   -----------------
   -- Check_Names --
   -----------------

   overriding procedure Check_Names
     (Expression    : in out Property_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment)
   is
   begin
      Expression.Value.Check_Names (Environment);
   end Check_Names;

   -----------------
   -- Check_Types --
   -----------------

   overriding procedure Check_Types
     (Expression    : in out Property_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type)
   is
      Name       : constant String :=
        Ada.Strings.Unbounded.To_String (Expression.Property);
      Value_Type : Tau.Types.Tau_Type;
   begin
      Expression.Value.Check_Types
        (Environment, Tau.Types.Any_Type, Value_Type);

      if not Value_Type.Has_Property (Name) then
         Ada.Text_IO.Put_Line
           (Environment.Name
            & ": property " & Name & " does not exist in type "
            & Value_Type.Name);

         Expression.Error
           ("no property '" & Name & "' for type " & Value_Type.Name);
         Found_Type := Expected_Type;
      elsif not Expected_Type.Is_Convertible_From
        (Value_Type.Property_Type (Name))
      then
         Expression.Error
           ("expected type " & Expected_Type.Name
            & " but found " & Value_Type.Property_Type (Name).Name);
         Found_Type := Expected_Type;
      else
         Found_Type := Value_Type.Property_Type (Name);
      end if;
   end Check_Types;

   --------------------------
   -- Property_Expression --
   --------------------------

   function Property_Expression
     (Position  : GCS.Positions.File_Position;
      Left      : Tau_Expression;
      Property  : String)
      return Tau_Expression
   is
      Expression : Property_Expression_Type := Property_Expression_Type'
        (Root_Tau_Expression with
         Value    => Left,
         Property => Ada.Strings.Unbounded.To_Unbounded_String (Property));
   begin
      Expression.Initialize_Node (Position);
      return new Property_Expression_Type'(Expression);
   end Property_Expression;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression : Property_Expression_Type;
      Generator  : in out Tau.Generators.Root_Tau_Generator'Class)
      return String
   is
      Name : constant String :=
               Ada.Strings.Unbounded.To_String (Expression.Property);
   begin
      return Expression.Value.To_String (Generator)
        & "." & Name;
   end To_String;

end Tau.Expressions.Properties;
