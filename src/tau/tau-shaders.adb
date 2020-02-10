with Tau.Declarations;

with Tau.Types.Standard;

package body Tau.Shaders is

   -----------
   -- Check --
   -----------

   function Check
     (Shader : Root_Tau_Shader)
      return Boolean
   is
      Env : constant Tau.Environment.Tau_Environment :=
        Tau.Environment.Create_Child
                (Tau.Environment.Global_Environment, Shader.Name);
   begin
      Root_Tau_Shader'Class (Shader).Check (Env);

      if Env.Has_Errors then
         Env.Write_Errors;
      end if;

      return not Env.Has_Errors;
   end Check;

   -----------
   -- Check --
   -----------

   procedure Check
     (Shader      : Root_Tau_Shader;
      Environment : Tau.Environment.Tau_Environment)
   is
      Shader_Env : constant Tau.Environment.Tau_Environment :=
                     Environment.Create_Child
                       (Shader.Name);
   begin

      Shader_Env.Set_Return_Type
        (Tau.Types.Standard.Tau_Vector (4));

      for Argument of Shader.Arguments loop
         Argument.Elaborate (Shader_Env);
      end loop;

      for Declaration of Shader.Declarations loop
         Declaration.Elaborate (Shader_Env);
      end loop;

      if not Shader_Env.Has_Errors then
         for Statement of Shader.Statements loop
            Statement.Check (Shader_Env);
         end loop;
      end if;

   end Check;

   -------------
   -- Compile --
   -------------

   function Compile
     (Shader     : Root_Tau_Shader;
      Bindings   : Tau.Environment.Tau_Environment;
      Generator  : in out Tau.Generators.Root_Tau_Generator'Class)
      return Boolean
   is
   begin
      Shader.Check (Bindings);

      if Bindings.Has_Errors then
         return False;
      end if;

      Generator.Start_Shader (Shader.Name, Shader.Stage, Bindings);

      for Argument of Shader.Arguments loop
         Argument.Compile (Generator);
      end loop;

      for Declaration of Shader.Declarations loop
         Declaration.Compile (Generator);
      end loop;

      for Statement of Shader.Statements loop
         Statement.Compile (Generator);
      end loop;

      Generator.End_Shader;
      return True;
   end Compile;

end Tau.Shaders;
