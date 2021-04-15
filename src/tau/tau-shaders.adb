package body Tau.Shaders is

   --------------------------
   -- Iterate_Declarations --
   --------------------------

   procedure Iterate_Declarations
     (Shader  : Root_Tau_Shader_Stage'Class;
      Class   : Stage_Declaration_Class;
      Process : not null access procedure
        (Declaration : Tau.Declarations.Tau_Declaration))
   is
   begin
      for Declaration of Shader.Declarations (Class) loop
         Process (Declaration);
      end loop;
   end Iterate_Declarations;

   ------------------------
   -- Iterate_Statements --
   ------------------------

   procedure Iterate_Statements
     (Shader : Root_Tau_Shader_Stage'Class;
      Process : not null access
        procedure (Statement : Tau.Statements.Tau_Statement))
   is
   begin
      for Statement of Shader.Statements loop
         Process (Statement);
      end loop;
   end Iterate_Statements;

end Tau.Shaders;
