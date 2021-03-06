with Ada.Strings.Unbounded;

with Tau.Entries;

package body Tau.Expressions.References is

   type Reference_Expression_Type is
     new Root_Tau_Expression with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Check_Names
     (Expression    : in out Reference_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment);

   overriding procedure Check_Types
     (Expression    : in out Reference_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type);

   overriding function To_String
     (Expression : Reference_Expression_Type;
      Generator  : Tau.Generators.Root_Tau_Generator'Class)
      return String;

   overriding function Depends_On
     (Node : Reference_Expression_Type;
      Name : String)
      return Boolean
   is (-Node.Name = Name);

   -----------------
   -- Check_Names --
   -----------------

   overriding procedure Check_Names
     (Expression    : in out Reference_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment)
   is
      Name : constant String :=
        Ada.Strings.Unbounded.To_String (Expression.Name);
   begin
      if not Environment.Contains (Name) then
         Expression.Error
           ("undefined: " & Name);
      end if;
   end Check_Names;

   -----------------
   -- Check_Types --
   -----------------

   overriding procedure Check_Types
     (Expression    : in out Reference_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type)
   is
      Name : constant String :=
        Ada.Strings.Unbounded.To_String (Expression.Name);
      Ref  : constant Tau.Entries.Tau_Entry :=
        Environment.Get (Name);
   begin
      if not Ref.Is_Value_Entry then
         Expression.Error
           ("expected a value but found " & Ref.Name);
         Found_Type := Expected_Type;
      else
         Found_Type := Ref.Entry_Type;
         if not Expected_Type.Is_Convertible_From (Found_Type) then
            Expression.Error
              ("expected type " & Expected_Type.Name
               & " but found " & Found_Type.Name);
         end if;
      end if;
   end Check_Types;

   --------------------------
   -- Reference_Expression --
   --------------------------

   function Reference_Expression
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Tau_Expression
   is
      Expression : Reference_Expression_Type := Reference_Expression_Type'
        (Root_Tau_Expression with
         Name => Ada.Strings.Unbounded.To_Unbounded_String (Name));
   begin
      Expression.Initialize_Node (Position);
      return new Reference_Expression_Type'(Expression);
   end Reference_Expression;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression : Reference_Expression_Type;
      Generator  : Tau.Generators.Root_Tau_Generator'Class)
      return String
   is
      Name : constant String :=
               Ada.Strings.Unbounded.To_String (Expression.Name);
   begin
      return Generator.Normalize_Reference (Name);
   end To_String;

end Tau.Expressions.References;
