with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Rho;

with Tau.Parser.Tokens;                use Tau.Parser.Tokens;
with Tau.Parser.Lexical;               use Tau.Parser.Lexical;

with Tau.Expressions;

with Tau.Material.Create;

with Tau.Declarations.Lists;
with Tau.Statements.Lists;

with Tau.Shaders.Create;
with Tau.Shaders.Lists;

package body Tau.Parser is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   function Parse_Top_Level_Declaration return Tau.Objects.Tau_Object;

   function Parse_Material_Declaration
     (Is_Generic  : Boolean := False;
      Is_Abstract : Boolean := False;
      Formals     : Tau.Declarations.Lists.List :=
        Tau.Declarations.Lists.Empty_List)
      return Tau.Material.Tau_Material;

   function Parse_Qualified_Name return String;

   procedure Scan_Matching_Qualified_Name (Match_Name : String);

   type Object_Declaration_Context is
     (Formal_Argument_Context,
      Generic_Formal_Context,
      Local_Declaration_Context);

   function Parse_Object_Declarations
     (Context : Object_Declaration_Context)
      return Tau.Declarations.Lists.List;

   function Parse_Shader_List return Tau.Shaders.Lists.List;

   function Parse_Shader return Tau.Shaders.Tau_Shader;

   function Parse_Expression return Tau.Expressions.Tau_Expression;

   function Parse_Statement_List return Tau.Statements.Lists.List;

   function Parse_Statement return Tau.Statements.Tau_Statement;

   function At_Statement return Boolean
   is (Tok = Tok_Identifier or else Tok = Tok_Return);

   type Operator_Precedence is range 1 .. 9;

   type Operator_Associativity is (Left);

   type Infix_Operator_Function is access
     function (Left, Right : Tau.Expressions.Tau_Expression)
               return Tau.Expressions.Tau_Expression;

   type Infix_Operator_Info is
      record
         Valid         : Boolean := False;
         Precedence    : Operator_Precedence := Operator_Precedence'Last;
         Associativity : Operator_Associativity := Left;
         Fn            : Infix_Operator_Function := null;
      end record;

   type Infix_Operator_Array is array (Token) of Infix_Operator_Info;

   Infix_Operator : constant Infix_Operator_Array :=
     (Tok_Plus     => (True, 4, Left, Tau.Expressions.Add'Access),
      Tok_Minus    => (True, 4, Left, Tau.Expressions.Subtract'Access),
      Tok_Asterisk => (True, 3, Left, Tau.Expressions.Multiply'Access),
      Tok_Slash    => (True, 3, Left, Tau.Expressions.Divide'Access),
      others       => (others => <>));

   ---------------
   -- Load_File --
   ---------------

   function Load_File (Path : String) return Tau.Objects.Tau_Object is
   begin
      Open (Path);

      return Result : constant Tau.Objects.Tau_Object :=
        Parse_Top_Level_Declaration
      do
         Close;
      end return;
   end Load_File;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression return Tau.Expressions.Tau_Expression is

      function Parse_Primary
        return Tau.Expressions.Tau_Expression;

      function Parse_Function_Call
        return Tau.Expressions.Tau_Expression
        with Pre => Tok = Tok_Identifier and then Next_Tok = Tok_Left_Paren;

      function Parse_Operator_Expression
        (Precedence : Operator_Precedence)
         return Tau.Expressions.Tau_Expression;

      function Parse_Function_Call
        return Tau.Expressions.Tau_Expression
      is
         Position : constant GCS.Positions.File_Position :=
           GCS.Positions.Get_Current_Position;
         Name     : constant String := Tok_Text;

         function Parse_Arguments
           return Tau.Expressions.Tau_Expression_Array;

         ---------------------
         -- Parse_Arguments --
         ---------------------

         function Parse_Arguments
           return Tau.Expressions.Tau_Expression_Array
         is
            use type Tau.Expressions.Tau_Expression_Array;
            Argument : constant Tau.Expressions.Tau_Expression :=
              Parse_Expression;
         begin
            if Tok = Tok_Comma then
               Scan;
               return Argument & Parse_Arguments;
            else
               if Tok = Tok_Right_Paren then
                  Scan;
               else
                  Error ("missing ')'");
               end if;
               return (1 => Argument);
            end if;
         end Parse_Arguments;

      begin
         Scan;
         Scan;

         return Tau.Expressions.Apply (Position, Name, Parse_Arguments);
      end Parse_Function_Call;

      -------------------------------
      -- Parse_Operator_Expression --
      -------------------------------

      function Parse_Operator_Expression
        (Precedence : Operator_Precedence)
         return Tau.Expressions.Tau_Expression
      is
         Left : Tau.Expressions.Tau_Expression :=
           (if Precedence = Operator_Precedence'First
            then Parse_Primary
            else Parse_Operator_Expression (Precedence - 1));
         Op   : Infix_Operator_Info := Infix_Operator (Tok);
      begin
         while Op.Valid and then Op.Precedence = Precedence loop
            Scan;

            declare
               Right : constant Tau.Expressions.Tau_Expression :=
                 (if Precedence = Operator_Precedence'First
                  then Parse_Primary
                  else Parse_Operator_Expression (Precedence - 1));
            begin
               Left := Op.Fn (Left, Right);
            end;

            Op := Infix_Operator (Tok);

         end loop;
         return Left;
      end Parse_Operator_Expression;

      -------------------
      -- Parse_Primary --
      -------------------

      function Parse_Primary
        return Tau.Expressions.Tau_Expression
      is
         Primary : Tau.Expressions.Tau_Expression;
         Position : constant GCS.Positions.File_Position :=
           GCS.Positions.Get_Current_Position;
      begin
         if Tok = Tok_Left_Paren then
            Scan;
            Primary := Parse_Expression;
            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
         elsif Tok = Tok_Integer_Constant then
            Primary :=
              Tau.Expressions.Literal (Position, Integer'Value (Tok_Text));
            Scan;
         elsif Tok = Tok_Floating_Point_Constant then
            Primary :=
              Tau.Expressions.Literal (Position, Float'Value (Tok_Text));
            Scan;
         elsif Tok = Tok_Identifier then
            if Next_Tok = Tok_Left_Paren then
               Primary := Parse_Function_Call;
            else
               Primary :=
                 Tau.Expressions.Reference (Position, Tok_Text);
               Scan;
            end if;
         else
            Error ("expected an expression");
            Scan;
         end if;

         while Tok = Tok_Dot loop
            Scan;

            if Tok = Tok_Identifier then
               Primary :=
                 Tau.Expressions.Property
                   (Position, Primary, Tok_Text);
               Scan;
            else
               Error ("missing identifier");
            end if;
         end loop;

         return Primary;
      end Parse_Primary;

   begin
      return Parse_Operator_Expression
        (Operator_Precedence'Last);
   end Parse_Expression;

   --------------------------------
   -- Parse_Material_Declaration --
   --------------------------------

   function Parse_Material_Declaration
     (Is_Generic  : Boolean := False;
      Is_Abstract : Boolean := False;
      Formals     : Tau.Declarations.Lists.List :=
        Tau.Declarations.Lists.Empty_List)
      return Tau.Material.Tau_Material
   is
   begin

      if Tok /= Tok_Material then
         Error ("expected 'material'");
         return null;
      end if;

      Scan;

      declare
         Position  : constant GCS.Positions.File_Position :=
                       GCS.Positions.Get_Current_Position;
         Name      : constant String := Parse_Qualified_Name;
         Arguments : Tau.Declarations.Lists.List;
         Shaders   : Tau.Shaders.Lists.List;
      begin
         if Name = "" then
            return null;
         end if;

         if Tok = Tok_Left_Paren then
            Scan;
            Arguments := Parse_Object_Declarations (Formal_Argument_Context);
            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
         end if;

         if Tok = Tok_Is then
            Scan;
         else
            Error ("missing 'is'");
         end if;

         Shaders := Parse_Shader_List;

         if Tok = Tok_End then
            Scan;
            Scan_Matching_Qualified_Name (Name);
         else
            Error ("missing 'end " & Name & "'");
         end if;

         if Tok = Tok_Semicolon then
            Scan;
         else
            Error ("missing ';'");
         end if;

         if Tok /= Tok_End_Of_File then
            Error ("extra tokens ignored");
         end if;

         return Tau.Material.Create.New_Material
           (Declaration     => Position,
            Name            => Name,
            Is_Generic      => Is_Generic,
            Is_Abstract     => Is_Abstract,
            Generic_Formals => Formals,
            Arguments       => Arguments,
            Shaders         => Shaders);
      end;

   end Parse_Material_Declaration;

   -------------------------------
   -- Parse_Object_Declarations --
   -------------------------------

   function Parse_Object_Declarations
     (Context : Object_Declaration_Context)
      return Tau.Declarations.Lists.List
   is
      Result    : Tau.Declarations.Lists.List;
   begin
      while Tok = Tok_Identifier loop
         declare
            Names    : String_Lists.List;
            Qualifier : Rho.Storage_Qualifier := Rho.Input;
            Position : constant GCS.Positions.File_Position :=
                         GCS.Positions.Get_Current_Position;
         begin
            while Tok = Tok_Identifier loop
               Names.Append (Tok_Text);
               Scan;
               if Tok = Tok_Comma then
                  Scan;
                  if Tok = Tok_Colon then
                     Error ("extra ',' ignored");
                  elsif Tok /= Tok_Identifier then
                     Error ("syntax error");
                  end if;
               elsif Tok = Tok_Identifier then
                  Error ("missing ','");
               end if;
            end loop;

            if Tok = Tok_Colon then
               Scan;
            else
               Error ("missing ':'");
            end if;

            if Tok = Tok_In
              or else Tok = Tok_Out
              or else Tok = Tok_Constant
            then
               if Context /= Formal_Argument_Context then
                  Error ("storage qualifier not allowed here");
               end if;
               if Tok = Tok_In then
                  Qualifier := Rho.Input;
               elsif Tok = Tok_Out then
                  Qualifier := Rho.Output;
               elsif Tok = Tok_Constant then
                  Qualifier := Rho.Uniform;
               else
                  raise Constraint_Error with
                    "unimplemented: qualifier " & Tok_Text;
               end if;
               Scan;
            end if;

            if Tok /= Tok_Identifier then
               Error ("missing type name");
            else
               declare
                  Type_Name : constant String := Tok_Text;
                  Initialization : Tau.Expressions.Tau_Expression;
               begin
                  Scan;

                  if Tok = Tok_Becomes then
                     if Context /= Local_Declaration_Context then
                        Error ("initialization not allowed here");
                     end if;

                     Scan;
                     Initialization := Parse_Expression;
                  end if;

                  for Name of Names loop
                     declare
                        Dec : Tau.Declarations.Tau_Declaration;
                     begin
                        case Context is
                           when Formal_Argument_Context =>
                              Dec :=
                                Tau.Declarations.Global_Variable_Declaration
                                  (Position, Name, Qualifier, Type_Name);
                           when Generic_Formal_Context =>
                              Dec :=
                                Tau.Declarations.Generic_Formal_Declaration
                                  (Position, Name, Type_Name);
                           when Local_Declaration_Context =>
                              Dec :=
                                Tau.Declarations.Local_Variable_Declaration
                                  (Position, Name, Type_Name,
                                  Initialization);
                        end case;
                        Result.Append (Dec);
                     end;
                  end loop;
               end;
            end if;

            if Tok = Tok_Semicolon then
               Scan;
               if Context = Formal_Argument_Context
                 and then Tok /= Tok_Identifier
               then
                  Error ("missing declaration");
               end if;
            elsif Tok = Tok_Identifier then
               Error ("missing ';'");
            end if;
         end;
      end loop;

      return Result;

   end Parse_Object_Declarations;

   --------------------------
   -- Parse_Qualified_Name --
   --------------------------

   function Parse_Qualified_Name return String is

      function Internal_Parse_Name return String
        with Pre => Tok = Tok_Identifier;

      -------------------------
      -- Internal_Parse_Name --
      -------------------------

      function Internal_Parse_Name return String is
         Name : constant String := Tok_Text;
      begin
         Scan;
         if Tok = Tok_Dot then
            Scan;
            if Tok = Tok_Identifier then
               return Name & "." & Internal_Parse_Name;
            else
               Error ("missing identifier");
               return Name;
            end if;
         else
            return Name;
         end if;
      end Internal_Parse_Name;

   begin
      if Tok = Tok_Identifier then
         return Internal_Parse_Name;
      else
         Error ("missing name");
         return "";
      end if;
   end Parse_Qualified_Name;

   ------------------
   -- Parse_Shader --
   ------------------

   function Parse_Shader return Tau.Shaders.Tau_Shader is
   begin
      if Tok /= Tok_Function then
         Error ("expected 'function'");
         return null;
      end if;

      Scan;

      declare
         Position : constant GCS.Positions.File_Position :=
           GCS.Positions.Get_Current_Position;
         Name     : constant String := Tok_Text;
         Stage    : constant Rho.Shader_Stage :=
           (if Name = "fragment_shader"
            then Rho.Fragment_Shader
            else Rho.Vertex_Shader);
         Arguments : Tau.Declarations.Lists.List;
      begin
         Scan;

         if Tok = Tok_Left_Paren then
            Scan;

            Arguments := Parse_Object_Declarations (Formal_Argument_Context);

            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
         end if;

         if Tok /= Tok_Return then
            Error ("missing 'return'");
         end if;

         Scan;

         declare
            Return_Type_Name : constant String := Tok_Text;
            pragma Unreferenced (Return_Type_Name);
            Statements       : Tau.Statements.Lists.List;
            Declarations     : Tau.Declarations.Lists.List;
         begin

            Scan;

            if Tok = Tok_Is then
               Scan;
            else
               Error ("missing 'is'");
            end if;

            if Tok = Tok_Left_Paren then
               Scan;

               declare
                  Position   : constant GCS.Positions.File_Position :=
                    GCS.Positions.Get_Current_Position;
                  Expression : constant Tau.Expressions.Tau_Expression :=
                    Parse_Expression;
               begin
                  if Tok = Tok_Right_Paren then
                     Scan;
                  else
                     Error ("missing ')'");
                  end if;

                  Statements.Append
                    (Tau.Statements.Return_Statement (Position, Expression));
               end;

            else

               Declarations :=
                 Parse_Object_Declarations (Local_Declaration_Context);

               if Tok /= Tok_Begin then
                  Error ("missing 'begin'");
               else
                  Scan;
               end if;

               Statements := Parse_Statement_List;

               if Tok /= Tok_End then
                  Error ("missing 'end " & Name & "'");
               else
                  Scan;

                  if Tok /= Tok_Identifier then
                     Error ("missing '" & Name & "'");
                  else
                     if Tok_Text /= Name then
                        Error ("name does not match '" & Name & "'");
                     end if;
                     Scan;
                  end if;

               end if;

            end if;

            if Tok = Tok_Semicolon then
               Scan;
            else
               Error ("missing ';'");
            end if;

            return Tau.Shaders.Create.New_Shader
              (Declaration  => Position,
               Name         => Name,
               Stage        => Stage,
               Arguments    => Arguments,
               Declarations => Declarations,
               Statements   => Statements);
         end;
      end;
   end Parse_Shader;

   -----------------------
   -- Parse_Shader_List --
   -----------------------

   function Parse_Shader_List return Tau.Shaders.Lists.List is
      List : Tau.Shaders.Lists.List;
   begin
      while Tok = Tok_Function loop
         List.Append (Parse_Shader);
      end loop;
      return List;
   end Parse_Shader_List;

   ---------------------
   -- Parse_Statement --
   ---------------------

   function Parse_Statement return Tau.Statements.Tau_Statement is
      Position : constant GCS.Positions.File_Position :=
        GCS.Positions.Get_Current_Position;
      Statement : Tau.Statements.Tau_Statement;
   begin
      if Tok = Tok_Return then
         Scan;
         Statement :=
           Tau.Statements.Return_Statement
             (Position, Parse_Expression);
      elsif Tok = Tok_Identifier then
         declare
            Name : constant String := Tok_Text;
            Value : Tau.Expressions.Tau_Expression;
         begin
            Scan;
            if Tok = Tok_Becomes then
               Scan;
            else
               Error ("expected ':='");
            end if;

            Value := Parse_Expression;

            Statement :=
              Tau.Statements.Assignment_Statement
                (Position, Name, Value);
         end;

      else

         Error ("expected a statement");
         while Tok /= Tok_End
              and then Tok /= Tok_Semicolon
           and then Tok /= Tok_End_Of_File
         loop
            Scan;
         end loop;

      end if;

      if Tok = Tok_Semicolon then
         Scan;
      else
         Error ("missing ';'");
      end if;

      return Statement;
   end Parse_Statement;

   --------------------------
   -- Parse_Statement_List --
   --------------------------

   function Parse_Statement_List return Tau.Statements.Lists.List is
      List : Tau.Statements.Lists.List;
   begin
      while At_Statement loop
         List.Append (Parse_Statement);
      end loop;
      return List;
   end Parse_Statement_List;

   ---------------------------------
   -- Parse_Top_Level_Declaration --
   ---------------------------------

   function Parse_Top_Level_Declaration return Tau.Objects.Tau_Object is
      Is_Abstract : Boolean := False;
      Is_Generic  : Boolean := False;
      Generic_Formals : Tau.Declarations.Lists.List;
   begin
      if Tok = Tok_Abstract then
         Is_Abstract := True;
         Scan;
      end if;

      if Tok = Tok_Generic then
         Is_Generic := True;
         Scan;
         Generic_Formals :=
           Parse_Object_Declarations (Generic_Formal_Context);
      end if;

      if Tok = Tok_Material then
         return Tau.Objects.Tau_Object
           (Parse_Material_Declaration
              (Is_Generic  => Is_Generic,
               Is_Abstract => Is_Abstract,
               Formals     => Generic_Formals));
      else
         Error ("expected a top level declaration");
         return null;
      end if;
   end Parse_Top_Level_Declaration;

   ----------------------------------
   -- Scan_Matching_Qualified_Name --
   ----------------------------------

   procedure Scan_Matching_Qualified_Name (Match_Name : String) is
      Position : constant GCS.Positions.File_Position :=
                   GCS.Positions.Get_Current_Position;
      Name     : constant String := Parse_Qualified_Name;
   begin
      if Name /= Match_Name then
         Error (Position, "expected '" & Match_Name & "'");
      end if;
   end Scan_Matching_Qualified_Name;

end Tau.Parser;
