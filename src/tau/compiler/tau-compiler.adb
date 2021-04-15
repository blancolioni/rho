with GCS.Errors;

with Tau.Expressions;
with Tau.Topological_Sort;

package body Tau.Compiler is

   procedure Save_Global_Declarations
     (To       : in out Stage_Record;
      Stage    : Tau.Shaders.Tau_Shader_Stage);

   procedure Save_Local_Declarations
     (To       : in out Stage_Record;
      Stage    : Tau.Shaders.Tau_Shader_Stage);

   procedure Save_Statements
     (To       : in out Stage_Record;
      Stage    : Tau.Shaders.Tau_Shader_Stage);

   procedure Create_Dependencies
     (Stage_Rec : in out Stage_Record);

   function Must_Precede
     (Left, Right : Tau.Declarations.Tau_Declaration)
      return Boolean
   is (Right.Get_Initializer.Depends_On (Left.Name));

   package Declaration_Sorting is
     new Tau.Declarations.Lists.Generic_Sorting
       (Must_Precede);

   function Must_Precede
     (Left, Right : Tau.Statements.Tau_Statement)
      return Boolean
   is (Right.Depends_On (Left.Target));

   package Statement_Sorting is
     new Tau.Statements.Lists.Generic_Sorting
       (Must_Precede);

   ----------------
   -- Add_Shader --
   ----------------

   procedure Add_Shader
     (Compiler : in out Tau_Compiler'Class;
      Shader   : Tau.Shaders.Tau_Shader)
   is
   begin
      for Current_Shader of Compiler.Shaders loop
         if Current_Shader.Name = Shader.Name then
            return;
         end if;
      end loop;

      Compiler.Shaders.Append (Shader);
      for Stage in Rho.Shader_Stage loop
         if Shader.Has_Stage (Stage) then
            Compiler.Stages (Stage).Append (Shader.Stage (Stage));
         end if;
      end loop;
   end Add_Shader;

   -------------------------
   -- Create_Dependencies --
   -------------------------

   procedure Create_Dependencies
     (Stage_Rec : in out Stage_Record)
   is
      package Target_Maps is
        new WL.String_Maps
          (Tau.Statements.Tau_Statement, Tau.Statements."=");
      Target_Map : Target_Maps.Map;

   begin
      for Statement of Stage_Rec.Statements loop
         declare
            Target : constant String := Statement.Target;
         begin
            if Target /= "" then
               if Stage_Rec.Dependencies.Contains (Target) then
                  GCS.Errors.Error
                    (Statement.Defined_At,
                     "duplicate assignment to " & Target);
                  GCS.Errors.Error
                    (Target_Map.Element (Target).Defined_At,
                     "original assignment");
               else
                  Stage_Rec.Dependencies.Insert
                    (Target, Tau.Statements.Lists.Empty_List);
                  Target_Map.Insert (Target, Statement);
               end if;
            end if;
         end;
      end loop;

      for Position in Stage_Rec.Dependencies.Iterate loop
         declare
            Name : constant String := Dependent_Maps.Key (Position);
            Element : constant Tau.Statements.Tau_Statement :=
                        Target_Map.Element (Name);
         begin
            for Target_Position in Target_Map.Iterate loop
               declare
                  Target : constant String :=
                             Target_Maps.Key (Target_Position);
                  Statement : constant Tau.Statements.Tau_Statement :=
                                Target_Maps.Element (Target_Position);
               begin
                  if Element.Depends_On (Target) then
                     Stage_Rec.Dependencies (Position).Append (Statement);
                  end if;
               end;
            end loop;
         end;
      end loop;

      declare
         Original : Tau.Statements.Lists.List :=
                      Stage_Rec.Statements;
         Sorted   : Tau.Statements.Lists.List;
         Last     : Tau.Statements.Lists.List;
      begin
         while not Original.Is_Empty loop
            declare
               Done : Tau.Statements.Lists.List;
            begin
               for Statement of Original loop
                  declare
                     Name : constant String := Statement.Target;
                     Deps : constant Tau.Statements.Lists.List :=
                              (if Name = ""
                               then Tau.Statements.Lists.Empty_List
                               else Stage_Rec.Dependencies.Element (Name));
                  begin
                     if Name = "" then
                        Last.Append (Statement);
                        Done.Append (Statement);
                     elsif Deps.Is_Empty then
                        Sorted.Append (Statement);
                        Done.Append (Statement);
                     end if;
                  end;
               end loop;

               if Done.Is_Empty then
                  raise Constraint_Error with
                    "dependency loop";
               end if;

               for Statement of Done loop
                  declare
                     Position : Tau.Statements.Lists.Cursor :=
                                  Original.Find (Statement);
                  begin
                     pragma Assert
                       (Tau.Statements.Lists.Has_Element (Position));
                     Original.Delete (Position);
                  end;
               end loop;

               for Deps of Stage_Rec.Dependencies loop
                  for Statement of Done loop
                     declare
                        use Tau.Statements.Lists;
                        Position : Cursor := Deps.Find (Statement);
                     begin
                        if Has_Element (Position) then
                           Deps.Delete (Position);
                        end if;
                     end;
                  end loop;
               end loop;
            end;
         end loop;

         Stage_Rec.Statements := Sorted;
         for Statement of Last loop
            Stage_Rec.Statements.Append (Statement);
         end loop;
      end;

   end Create_Dependencies;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Compiler  : Tau_Compiler'Class;
      Generator : in out Tau.Generators.Root_Tau_Generator'Class)
   is
   begin
      for Stage in Compiler.Records'Range loop
         Generator.Set_Shader_Stage (Stage);
         for Declaration of Compiler.Records (Stage).Uniform_Variables loop
            Declaration.Compile (Generator);
         end loop;
         for Declaration of Compiler.Records (Stage).In_Variables loop
            Declaration.Compile (Generator);
         end loop;
         for Declaration of Compiler.Records (Stage).Out_Variables loop
            Declaration.Compile (Generator);
         end loop;

         declare
            Locals : Tau.Declarations.Lists.List :=
                       Compiler.Records (Stage).Local_Constants;
         begin
            Declaration_Sorting.Sort (Locals);
            for Declaration of Locals loop
               Declaration.Compile (Generator);
            end loop;
         end;

         declare
            Stmts : Tau.Statements.Lists.List :=
                      Compiler.Records (Stage).Statements;
         begin
            Statement_Sorting.Sort (Stmts);
            for Statement of Stmts loop
               Statement.Compile (Generator);
            end loop;
         end;

      end loop;
   end Generate;

   ----------
   -- Link --
   ----------

   procedure Link (Compiler : in out Tau_Compiler'Class) is
   begin
      for Stage in Compiler.Stages'Range loop
         for Item of Compiler.Stages (Stage) loop
            Save_Global_Declarations (Compiler.Records (Stage), Item);
            Save_Local_Declarations (Compiler.Records (Stage), Item);
            Save_Statements (Compiler.Records (Stage), Item);
         end loop;

         for Statement of Compiler.Records (Stage).Aggregate_Map loop
            declare
               Target : constant String := Statement.Target;
               Dec    : constant Tau.Declarations.Tau_Declaration :=
                          Compiler.Records (Stage).Table.Element (Target);
            begin
               Dec.Set_Initializer (Statement.Assigned_Value);
            end;
         end loop;

         declare
            function Is_Dependent_On
              (Left, Right : Tau.Declarations.Tau_Declaration)
               return Boolean
            is (Left.Depends_On (Right.Name));

            package Declaration_Sorting is
              new Tau.Topological_Sort
                (Element_Type    => Tau.Declarations.Tau_Declaration,
                 Is_Dependent_On => Is_Dependent_On,
                 "="             => Tau.Declarations."=",
                 Element_Lists   => Tau.Declarations.Lists);
         begin
            Declaration_Sorting.Sort
              (Compiler.Records (Stage).Local_Constants);
         end;

         Create_Dependencies (Compiler.Records (Stage));

      end loop;

   end Link;

   ------------------------------
   -- Save_Global_Declarations --
   ------------------------------

   procedure Save_Global_Declarations
     (To       : in out Stage_Record;
      Stage    : Tau.Shaders.Tau_Shader_Stage)
   is
      procedure Save_In_Variable
        (Declaration : Tau.Declarations.Tau_Declaration);

      procedure Save_Out_Variable
        (Declaration : Tau.Declarations.Tau_Declaration);

      procedure Save_Uniform_Variable
        (Declaration : Tau.Declarations.Tau_Declaration);

      ----------------------
      -- Save_In_Variable --
      ----------------------

      procedure Save_In_Variable
        (Declaration : Tau.Declarations.Tau_Declaration)
      is
      begin
         To.In_Variables.Append (Declaration);
         To.Table.Insert (Declaration.Name, Declaration);
      end Save_In_Variable;

      -----------------------
      -- Save_Out_Variable --
      -----------------------

      procedure Save_Out_Variable
        (Declaration : Tau.Declarations.Tau_Declaration)
      is
      begin
         To.Out_Variables.Append (Declaration);
         To.Table.Insert (Declaration.Name, Declaration);
      end Save_Out_Variable;

      ---------------------------
      -- Save_Uniform_Variable --
      ---------------------------

      procedure Save_Uniform_Variable
        (Declaration : Tau.Declarations.Tau_Declaration)
      is
      begin
         To.Uniform_Variables.Append (Declaration);
         To.Table.Insert (Declaration.Name, Declaration);
      end Save_Uniform_Variable;

   begin
      Stage.Iterate_Declarations
        (Tau.Shaders.In_Variable, Save_In_Variable'Access);
      Stage.Iterate_Declarations
        (Tau.Shaders.Out_Variable, Save_Out_Variable'Access);
      Stage.Iterate_Declarations
        (Tau.Shaders.Uniform_Variable, Save_Uniform_Variable'Access);
   end Save_Global_Declarations;

   -----------------------------
   -- Save_Local_Declarations --
   -----------------------------

   procedure Save_Local_Declarations
     (To       : in out Stage_Record;
      Stage    : Tau.Shaders.Tau_Shader_Stage)
   is
      procedure Save_Declaration
        (Declaration : Tau.Declarations.Tau_Declaration);

      ----------------------
      -- Save_Declaration --
      ----------------------

      procedure Save_Declaration
        (Declaration : Tau.Declarations.Tau_Declaration)
      is
      begin
         To.Local_Constants.Append (Declaration);
         if To.Table.Contains (Declaration.Name) then
            GCS.Errors.Error
              (Declaration.Defined_At,
               "duplicate definitions of " & Declaration.Name);
            GCS.Errors.Error
              (To.Table.Element (Declaration.Name).Defined_At,
               "original declaration");
         else
            To.Table.Insert (Declaration.Name, Declaration);
         end if;
      end Save_Declaration;

   begin
      Stage.Iterate_Declarations
        (Tau.Shaders.Provide_Name, Save_Declaration'Access);
      Stage.Iterate_Declarations
        (Tau.Shaders.Local_Name, Save_Declaration'Access);
   end Save_Local_Declarations;

   ---------------------
   -- Save_Statements --
   ---------------------

   procedure Save_Statements
     (To       : in out Stage_Record;
      Stage    : Tau.Shaders.Tau_Shader_Stage)
   is

      procedure Save_Statement
        (Statement : Tau.Statements.Tau_Statement);

      ----------------------
      -- Save_Statement --
      ----------------------

      procedure Save_Statement
        (Statement : Tau.Statements.Tau_Statement)
      is
         Target : constant String := Statement.Target;
         Is_Aggregate : Boolean := False;
      begin
         if Target /= "" then
            if not To.Table.Contains (Target) then
               GCS.Errors.Error
                 (Statement.Defined_At,
                  "undefined: " & Target);
            else
               declare
                  Dec : constant Tau.Declarations.Tau_Declaration :=
                          To.Table.Element (Target);
               begin
                  if Dec.Has_Aspect ("aggregate") then
                     Is_Aggregate := True;
                     if not To.Aggregate_Map.Contains (Target) then
                        To.Aggregate_Map.Insert (Target, Statement);
                     else
                        To.Aggregate_Map.Replace
                          (Target,
                           Tau.Statements.Assignment_Statement
                             (Statement.Defined_At,
                              Target,
                              Tau.Expressions.Add
                                (To.Aggregate_Map.Element (Target)
                                 .Assigned_Value,
                                 Statement.Assigned_Value)));
                     end if;
                  end if;
               end;
            end if;
         end if;

         if not Is_Aggregate then
            To.Statements.Append (Statement);
         end if;
      end Save_Statement;

   begin
      Stage.Iterate_Statements (Save_Statement'Access);
   end Save_Statements;

end Tau.Compiler;
