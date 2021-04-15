with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with WL.String_Maps;

with Rho;

with Tau.Parser.Tokens;                use Tau.Parser.Tokens;
with Tau.Parser.Lexical;               use Tau.Parser.Lexical;

with Tau.Expressions;

with Tau.Declarations;
with Tau.Statements.Lists;

with Tau.Shaders.Builder;

package body Tau.Parser is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package String_Maps is
     new WL.String_Maps (String);

   function Parse_Top_Level_Declaration return Tau.Objects.Tau_Object;

   function Parse_Qualified_Name return String;

   procedure Scan_Matching_Qualified_Name (Match_Name : String);

   type Object_Declaration_Context is
     (Global_Context,
      Require_Context,
      Provide_Context,
      Local_Context);

   procedure Parse_Object_Declarations
     (Context : Object_Declaration_Context;
      Stage   : Tau.Shaders.Tau_Shader_Stage);

   function Parse_Shader
     (Is_Abstract : Boolean)
     return Tau.Shaders.Tau_Shader;

   function Parse_Shader_Stage return Tau.Shaders.Tau_Shader_Stage;

   function Parse_Expression return Tau.Expressions.Tau_Expression;

   function Parse_Statement_List return Tau.Statements.Lists.List;

   function Parse_Statement return Tau.Statements.Tau_Statement;

   function At_Statement return Boolean
   is (Tok = Tok_Identifier or else Tok = Tok_Return);

   procedure Parse_Property_List
     (Object : not null access Tau.Objects.Root_Tau_Object'Class)
     with Unreferenced;

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
                      (Tok_Equal =>
                         (True, 6, Left, Tau.Expressions.Equal'Access),
                       Tok_Not_Equal =>
                         (True, 6, Left, Tau.Expressions.Not_Equal'Access),
                       Tok_Plus      =>
                         (True, 4, Left, Tau.Expressions.Add'Access),
                       Tok_Minus     =>
                         (True, 4, Left, Tau.Expressions.Subtract'Access),
                       Tok_Asterisk  =>
                         (True, 3, Left, Tau.Expressions.Multiply'Access),
                       Tok_Slash     =>
                         (True, 3, Left, Tau.Expressions.Divide'Access),
                       Tok_Power     =>
                         (True, 2, Left, Tau.Expressions.Power'Access),
                       others        => (others => <>));

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

      function Parse_Conditional_Expression
         return Tau.Expressions.Tau_Expression;

      ----------------------------------
      -- Parse_Conditional_Expression --
      ----------------------------------

      function Parse_Conditional_Expression
        return Tau.Expressions.Tau_Expression
      is
         Position : constant GCS.Positions.File_Position :=
           GCS.Positions.Get_Current_Position;
         Condition, True_Value, False_Value : Tau.Expressions.Tau_Expression;
      begin
         pragma Assert (Tok = Tok_If);
         Scan;
         Condition := Parse_Expression;

         if Tok = Tok_Then then
            Scan;
         else
            Error ("missing 'then'");
         end if;

         True_Value := Parse_Expression;

         if Tok = Tok_Else then
            Scan;
         else
            Error ("missing 'else'");
         end if;

         False_Value := Parse_Expression;

         return Tau.Expressions.Condition
           (Position, Condition, True_Value, False_Value);
      end Parse_Conditional_Expression;

      -------------------------
      -- Parse_Function_Call --
      -------------------------

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
            if Tok = Tok_If then
               Primary := Parse_Conditional_Expression;
            else
               Primary := Parse_Expression;
            end if;
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

   -------------------------------
   -- Parse_Object_Declarations --
   -------------------------------

   procedure Parse_Object_Declarations
     (Context : Object_Declaration_Context;
      Stage   : Tau.Shaders.Tau_Shader_Stage)
   is
   begin
      while Tok = Tok_Identifier loop
         declare
            Names     : String_Lists.List;
            Qualifier : Rho.Storage_Qualifier := Rho.Input;
            Position  : constant GCS.Positions.File_Position :=
                          GCS.Positions.Get_Current_Position;
            Aspects   : String_Maps.Map;
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
              or else Tok = Tok_Uniform
            then
               if Context /= Global_Context then
                  Error ("storage qualifier not allowed here");
               end if;
               if Tok = Tok_In then
                  Qualifier := Rho.Input;
               elsif Tok = Tok_Out then
                  Qualifier := Rho.Output;
               elsif Tok = Tok_Uniform then
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
                     if Context = Global_Context then
                        Error ("initialization not allowed here");
                     end if;

                     Scan;
                     Initialization := Parse_Expression;
                  end if;

                  if Tok = Tok_With then
                     Scan;
                     if Tok = Tok_Identifier then
                        while Tok = Tok_Identifier loop
                           declare
                              Aspect_Name : constant String := Tok_Text;
                           begin
                              Scan;
                              if Tok = Tok_Right_Arrow then
                                 Scan;
                                 if Tok = Tok_Identifier then
                                    Aspects.Insert (Aspect_Name, Tok_Text);
                                    Scan;
                                 else
                                    Error ("missing aspect value");
                                 end if;
                              else
                                 Aspects.Insert (Aspect_Name, "true");
                              end if;
                              if Tok = Tok_Comma then
                                 Scan;
                                 if Tok /= Tok_Identifier then
                                    Error ("missing aspect");
                                 end if;
                              elsif Tok = Tok_Identifier then
                                 Error ("missing ','");
                              end if;
                           end;
                        end loop;
                     else
                        Error ("missing aspect");
                     end if;
                  end if;

                  for Name of Names loop
                     declare
                        use Tau.Shaders.Builder;
                        Dec : constant Tau.Declarations.Tau_Declaration :=
                                (if Context = Global_Context
                                 then Declarations.Global_Variable_Declaration
                                   (Position, Name, Qualifier, Type_Name)
                                 else Declarations.Constant_Declaration
                                   (Position, Name, Type_Name,
                                    Initialization));
                     begin
                        for Aspect in Aspects.Iterate loop
                           Dec.Add_Aspect
                             (Name  => String_Maps.Key (Aspect),
                              Value => String_Maps.Element (Aspect));
                        end loop;

                        case Context is
                           when Global_Context =>
                              case Qualifier is
                                 when Rho.Input =>
                                    Add_In_Variable (Stage, Dec);
                                 when Rho.Output =>
                                    Add_Out_Variable (Stage, Dec);
                                 when Rho.Uniform =>
                                    Add_Uniform_Variable (Stage, Dec);
                              end case;
                           when Require_Context =>
                              Add_Require (Stage, Dec);
                           when Provide_Context =>
                              Add_Provide (Stage, Dec);
                           when Local_Context =>
                              Add_Local (Stage, Dec);
                        end case;

                     end;
                  end loop;
               end;
            end if;

            if Tok = Tok_Semicolon then
               Scan;
            elsif Tok = Tok_Identifier then
               Error ("missing ';'");
            end if;
         end;
      end loop;

   end Parse_Object_Declarations;

   -------------------------
   -- Parse_Property_List --
   -------------------------

   procedure Parse_Property_List
     (Object : not null access Tau.Objects.Root_Tau_Object'Class)
   is
   begin
      while Tok = Tok_Identifier loop
         declare
            Position : constant GCS.Positions.File_Position :=
              Get_Current_Position;
            Property_Name : constant String := Tok_Text;
         begin
            Scan;
            if Tok = Tok_Equal then
               Scan;
            else
               Error ("missing '='");
            end if;

            if Tok = Tok_String_Constant then
               declare
                  Value : constant String := Tok_Text;
               begin
                  Scan;

                  if Object.Is_Valid_Property (Property_Name) then
                     Object.Set_Property (Property_Name, Value);
                  else
                     Error (Position,
                            "invalid property for "
                            & Object.Class_Name
                            & " "
                            & Object.Name
                            & ": "
                            & Property_Name);
                  end if;
               end;
            else
               Error ("only string properties are currently supported");
               Scan;
            end if;

            if Tok = Tok_Semicolon then
               Scan;
            end if;
         end;
      end loop;
   end Parse_Property_List;

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

   -----------------------
   -- Parse_Shader_List --
   -----------------------

   function Parse_Shader
     (Is_Abstract : Boolean)
      return Tau.Shaders.Tau_Shader
   is
      Position : constant GCS.Positions.File_Position :=
                   GCS.Positions.Get_Current_Position;
      Name : constant String := Parse_Qualified_Name;
      Shader : constant Tau.Shaders.Tau_Shader :=
                 Tau.Shaders.Builder.New_Shader
                   (Position, Name, Is_Abstract);
   begin
      if Tok = Tok_Colon then
         Scan;
         while Tok = Tok_Identifier loop
            declare
               Base : constant String := Parse_Qualified_Name
                 with Unreferenced;
            begin
               if Tok = Tok_Comma then
                  Scan;
                  if Tok /= Tok_Identifier then
                     Error ("extra ',' ignored");
                  end if;
               elsif Tok = Tok_Identifier then
                  Error ("missing ','");
               end if;
            end;
         end loop;
      end if;

      if Tok = Tok_Is then
         Scan;
      elsif Tok = Tok_Vertex
        or else Tok = Tok_Fragment
      then
         Error ("missing 'is'");
      else
         Error ("missing shader body");
         return Shader;
      end if;

      while Tok = Tok_Vertex or else Tok = Tok_Fragment loop
         Tau.Shaders.Builder.Add_Stage
           (Shader, Parse_Shader_Stage);
      end loop;

      if Tok = Tok_End then
         Scan;
         Scan_Matching_Qualified_Name (Name);
         if Tok = Tok_Semicolon then
            Scan;
         else
            Error ("missing ';'");
         end if;
      else
         Error ("missing 'end'");
      end if;

      return Shader;
   end Parse_Shader;

   ------------------------
   -- Parse_Shader_Stage --
   ------------------------

   function Parse_Shader_Stage return Tau.Shaders.Tau_Shader_Stage is
      Position : constant GCS.Positions.File_Position :=
                   GCS.Positions.Get_Current_Position;
      Stage    : constant Rho.Shader_Stage :=
                   (if Tok = Tok_Vertex
                    then Rho.Vertex_Shader
                    elsif Tok = Tok_Fragment
                    then Rho.Fragment_Shader
                    else (raise Constraint_Error with
                        "expected 'vertex' or 'fragment'"));
      Start_Tok : constant String := Tok_Text;
      Shader   : constant Tau.Shaders.Tau_Shader_Stage :=
                   Tau.Shaders.Builder.New_Shader_Stage
                     (Position => Position,
                      Stage    => Stage);
   begin
      Scan;
      Parse_Object_Declarations (Global_Context, Shader);

      while Tok = Tok_Require
        or else Tok = Tok_Provide
        or else Tok = Tok_Local
      loop
         declare
            Context : constant Object_Declaration_Context :=
                        (if Tok = Tok_Require then Require_Context
                         elsif Tok = Tok_Provide then Provide_Context
                         else Local_Context);
         begin
            Scan;
            Parse_Object_Declarations (Context, Shader);
         end;
      end loop;

      if Tok = Tok_Begin then
         Scan;
         declare
            Statements : constant Tau.Statements.Lists.List :=
                           Parse_Statement_List;
         begin
            for Statement of Statements loop
               Tau.Shaders.Builder.Add_Statement (Shader, Statement);
            end loop;
         end;
      end if;

      if Tok = Tok_End then
         Scan;
         if Tok_Text /= Start_Tok then
            Error ("expected '" & Start_Tok & "'");
         end if;
         if Tok = Tok_Fragment or else Tok = Tok_Vertex then
            Scan;
         end if;
      else
         Error ("missing 'end '" & Start_Tok);
      end if;
      if Tok = Tok_Semicolon then
         Scan;
      else
         Error ("missing ';'");
      end if;

      return Shader;
   end Parse_Shader_Stage;

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
   begin
      if Tok = Tok_Abstract then
         Is_Abstract := True;
         Scan;
      end if;

      if Tok = Tok_Shader then
         Scan;
         return Tau.Objects.Tau_Object (Parse_Shader (Is_Abstract));
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
