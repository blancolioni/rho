package body Tau.Shaders.Builder is

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
     (Shader   : Tau_Shader;
      Argument : Tau.Declarations.Tau_Declaration)
   is
   begin
      Shader.Arguments.Append (Argument);
   end Add_Argument;

   ----------------
   -- Add_Global --
   ----------------

   procedure Add_In_Variable
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration)
   is
   begin
      Stage.Declarations (In_Variable).Append (Declaration);
   end Add_In_Variable;

   ---------------
   -- Add_Local --
   ---------------

   procedure Add_Local
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration)
   is
   begin
      Stage.Declarations (Local_Name).Append (Declaration);
   end Add_Local;

   ----------------------
   -- Add_Out_Variable --
   ----------------------

   procedure Add_Out_Variable
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration)
   is
   begin
      Stage.Declarations (Out_Variable).Append (Declaration);
   end Add_Out_Variable;

   -----------------
   -- Add_Provide --
   -----------------

   procedure Add_Provide
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration)
   is
   begin
      Stage.Declarations (Provide_Name).Append (Declaration);
   end Add_Provide;

   -----------------
   -- Add_Require --
   -----------------

   procedure Add_Require
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration)
   is
   begin
      Stage.Declarations (Require_Name).Append (Declaration);
   end Add_Require;

   ---------------
   -- Add_Stage --
   ---------------

   procedure Add_Stage
     (Shader : Tau_Shader;
      Stage  : Tau_Shader_Stage)
   is
   begin
      Shader.Stages (Stage.Stage) := Stage;
   end Add_Stage;

   -------------------
   -- Add_Statement --
   -------------------

   procedure Add_Statement
     (Stage     : Tau_Shader_Stage;
      Statement : Tau.Statements.Tau_Statement)
   is
   begin
      Stage.Statements.Append (Statement);
   end Add_Statement;

   --------------------------
   -- Add_Uniform_Variable --
   --------------------------

   procedure Add_Uniform_Variable
     (Stage       : Tau_Shader_Stage;
      Declaration : Tau.Declarations.Tau_Declaration)
   is
   begin
      Stage.Declarations (Uniform_Variable).Append (Declaration);
   end Add_Uniform_Variable;

   ----------------
   -- New_Shader --
   ----------------

   function New_Shader
     (Position    : GCS.Positions.File_Position;
      Name        : String;
      Is_Abstract : Boolean)
      return Tau_Shader
   is
   begin
      return Shader : constant Tau_Shader :=
        new Root_Tau_Shader'
          (Tau.Objects.Root_Tau_Object with
             Is_Abstract => Is_Abstract,
             Inherits    => <>,
             Arguments   => <>,
             Stages      => <>)
      do
         Shader.Initialize_Object (Position, Name);
      end return;
   end New_Shader;

   ----------------------
   -- New_Shader_Stage --
   ----------------------

   function New_Shader_Stage
     (Position : GCS.Positions.File_Position;
      Stage    : Rho.Shader_Stage)
      return Tau_Shader_Stage
   is
   begin
      return Shader_Stage : constant Tau_Shader_Stage :=
        new Root_Tau_Shader_Stage'
          (Tau.Objects.Root_Tau_Object with
             Stage      => Stage,
             Declarations => <>,
             Statements   => <>)
      do
         Shader_Stage.Initialize_Object (Position, "");
      end return;
   end New_Shader_Stage;

end Tau.Shaders.Builder;
