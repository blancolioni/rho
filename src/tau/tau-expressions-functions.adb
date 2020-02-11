with Ada.Strings.Unbounded;

with Tau.Entries;
with Tau.Function_Objects;

package body Tau.Expressions.Functions is

   type Function_Expression_Type is
     new Root_Tau_Expression with
      record
         Function_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Function_Entry : Tau.Entries.Tau_Entry;
         Values         : Expression_Array_Holders.Holder;
      end record;

   overriding function Children
     (Expression : Function_Expression_Type)
      return Tau_Node_Array;

   overriding procedure Check_Names
     (Expression    : in out Function_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment);

   overriding procedure Check_Types
     (Expression    : in out Function_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type);

   overriding function To_String
     (Expression : Function_Expression_Type;
      Generator  : in out Tau.Generators.Root_Tau_Generator'Class)
      return String;

   -----------
   -- Apply --
   -----------

   function Apply
     (Position       : GCS.Positions.File_Position;
      Function_Name  : String;
      Values         : Tau_Expression_Array)
      return Tau_Expression
   is
      Expression : Function_Expression_Type :=
        Function_Expression_Type'
          (Root_Tau_Expression with
           Function_Name  =>
             Ada.Strings.Unbounded.To_Unbounded_String (Function_Name),
           Function_Entry => null,
           Values         =>
             Expression_Array_Holders.To_Holder (Values));
   begin
      Expression.Initialize_Node (Position);
      return new Function_Expression_Type'(Expression);
   end Apply;

   -----------------
   -- Check_Names --
   -----------------

   overriding procedure Check_Names
     (Expression    : in out Function_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment)
   is
      Function_Name : constant String :=
        Ada.Strings.Unbounded.To_String (Expression.Function_Name);
      Arguments     : constant Tau_Expression_Array :=
        Expression.Values.Element;
   begin
      if not Environment.Contains (Function_Name) then
         Expression.Error
           ("undefined: " & Function_Name);
      end if;

      for Argument of Arguments loop
         Argument.Check_Names (Environment);
      end loop;

   end Check_Names;

   -----------------
   -- Check_Types --
   -----------------

   overriding procedure Check_Types
     (Expression    : in out Function_Expression_Type;
      Environment   : Tau.Environment.Tau_Environment;
      Expected_Type : Tau.Types.Tau_Type;
      Found_Type    : out Tau.Types.Tau_Type)
   is
      Function_Name : constant String :=
        Ada.Strings.Unbounded.To_String (Expression.Function_Name);
      Object        : constant Tau.Entries.Tau_Entry :=
        Environment.Get (Function_Name);
   begin
      Expression.Function_Entry := Object;
      if Object.Is_Type_Entry then
         declare
            Values : constant Tau_Expression_Array :=
              Expression.Values.Element;
            Types  : Tau.Types.Tau_Type_Array (Values'Range);
         begin
            for I in Values'Range loop
               Values (I).Check_Types
                 (Environment, Tau.Types.Any_Type, Types (I));
            end loop;

            if not Environment.Has_Errors then
               if not Object.Entry_Type
                 .Is_Convertible_From_Vector (Types)
               then
                  Expression.Error
                    ("invalid values for type "
                     & Object.Entry_Type.Name);
               end if;
            end if;

            if not Expected_Type
              .Is_Convertible_From (Object.Entry_Type)
            then
               Expression.Error
                 ("expected type " & Expected_Type.Name
                  & " but found "
                  & Object.Entry_Type.Name);
               Found_Type := Expected_Type;
            else
               Found_Type := Object.Entry_Type;
            end if;
         end;
      elsif Object.Is_Function_Entry then
         declare
            Formal_Arguments   : constant Tau.Types.Tau_Type_Array :=
              Object.Entry_Type.Map_From;
            Result_Type        : constant Tau.Types.Tau_Type :=
              Object.Entry_Type.Map_To;
            Required_Count     : constant Natural := Formal_Arguments'Length;
            Supplied_Arguments : constant Tau_Expression_Array :=
              Expression.Values.Element;
         begin
            if Supplied_Arguments'Length < Required_Count then
               Expression.Error
                 ("insufficient arguments to function "
                  & Function_Name
                  & "; expected"
                  & Required_Count'Image
                  & " but found"
                  & Natural'Image
                    (Supplied_Arguments'Length));
            elsif Supplied_Arguments'Length > Required_Count then
               Expression.Error
                 ("too many arguments to function "
                  & Function_Name
                  & "; expected"
                  & Required_Count'Image
                  & " but found"
                  & Natural'Image
                    (Supplied_Arguments'Length));
            else
               for I in Supplied_Arguments'Range loop
                  Supplied_Arguments (I).Check
                    (Environment, Formal_Arguments (I));
               end loop;
            end if;

            Found_Type := Result_Type;
            if not Expected_Type.Is_Convertible_From (Found_Type) then
               Expression.Error
                 ("expected type " & Expected_Type.Name
                  & " but found " & Found_Type.Name);
            end if;
         end;
      else
         Expression.Error
           ("expected a function or type but found " & Object.Name);
         Found_Type := Expected_Type;
      end if;
   end Check_Types;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Expression : Function_Expression_Type)
      return Tau_Node_Array
   is
      Base_Children : constant Tau_Node_Array :=
        Root_Tau_Expression (Expression).Children;
      Arguments  : constant Tau.Expressions.Tau_Expression_Array :=
        Expression.Values.Element;
      Arg_Children  : Tau_Node_Array (Arguments'Range);

   begin

      for I in Arg_Children'Range loop
         Arg_Children (I) := Tau_Node (Arguments (I));
      end loop;

      return Base_Children & Arg_Children;
   end Children;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Expression : Function_Expression_Type;
      Generator  : in out Tau.Generators.Root_Tau_Generator'Class)
         return String
   is
      function Image (Agg : Tau_Expression_Array) return String
      is (if Agg'Length = 0
          then ""
          elsif Agg'Length = 1
          then Agg (Agg'First).To_String (Generator)
          else Agg (Agg'First).To_String (Generator) & ","
          & Image (Agg (Agg'First + 1 .. Agg'Last)));

      Name : constant String :=
        (if Expression.Function_Entry.Is_Type_Entry
         then Generator.Shader_Type_Name
           (Expression.Function_Entry.Name)
         else Expression.Function_Entry.Name);
   begin
      return Name & "(" & Image (Expression.Values.Element) & ")";
   end To_String;

end Tau.Expressions.Functions;
