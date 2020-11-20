with Tau.Declarations;

with Tau.Types.Standard;

package body Tau.Shaders is

   procedure Compile_Support
     (Shader    : Root_Tau_Shader'Class;
      Generator : in out Tau.Generators.Root_Tau_Generator'Class);

   function Compile_Evaluation
     (Shader    : Root_Tau_Shader'Class;
      Bindings   : Tau.Environment.Tau_Environment;
      Generator  : in out Tau.Generators.Root_Tau_Generator'Class)
      return String;

   ----------
   -- Bind --
   ----------

   function Bind
     (Shader   : Root_Tau_Shader;
      Bindings : Tau.Environment.Tau_Environment)
      return Tau_Shader
   is
   begin
      return Result : constant Tau_Shader :=
        new Root_Tau_Shader'
          (Shader with delta Bindings => Bindings)
      do
         Result.Value := Tau.Values.Object_Value (Result);
         Result.Initialize_Object (Shader.Defined_At, Shader.Name);
      end return;
   end Bind;

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

      return not Shader.Has_Errors;
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

   --------------
   -- Children --
   --------------

   overriding function Children
     (Shader : Root_Tau_Shader)
      return Tau_Node_Array
   is
      function Declaration_List_Children
        (List : Tau.Declarations.Lists.List)
         return Tau_Node_Array;

      function Statement_List_Children
        (List : Tau.Statements.Lists.List)
         return Tau_Node_Array;

      -------------------------------
      -- Declaration_List_Children --
      -------------------------------

      function Declaration_List_Children
        (List : Tau.Declarations.Lists.List)
         return Tau_Node_Array
      is
         Count  : constant Natural := Natural (List.Length);
         Index  : Positive := 1;
      begin
         return Result : Tau_Node_Array (1 .. Count) do
            for Child of List loop
               Result (Index) := Tau_Node (Child);
               Index := Index + 1;
            end loop;
         end return;
      end Declaration_List_Children;

      -----------------------------
      -- Statement_List_Children --
      -----------------------------

      function Statement_List_Children
        (List : Tau.Statements.Lists.List)
         return Tau_Node_Array
      is
         Count  : constant Natural := Natural (List.Length);
         Index  : Positive := 1;
      begin
         return Result : Tau_Node_Array (1 .. Count) do
            for Child of List loop
               Result (Index) := Tau_Node (Child);
               Index := Index + 1;
            end loop;
         end return;
      end Statement_List_Children;

   begin
      return Tau.Objects.Root_Tau_Object (Shader).Children
        & Declaration_List_Children (Shader.Abstract_Arguments)
        & Declaration_List_Children (Shader.Arguments)
        & Declaration_List_Children (Shader.Declarations)
        & Statement_List_Children (Shader.Statements);
   end Children;

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

      Shader.Compile_Support (Generator);

      Generator.Return_Value (Shader.Return_Value.To_String (Generator));

      Generator.End_Shader;
      return True;
   end Compile;

   ------------------------
   -- Compile_Evaluation --
   ------------------------

   function Compile_Evaluation
     (Shader    : Root_Tau_Shader'Class;
      Bindings   : Tau.Environment.Tau_Environment;
      Generator  : in out Tau.Generators.Root_Tau_Generator'Class)
      return String
   is
   begin
      Generator.Push_Environment (Bindings);

      Shader.Compile_Support (Generator);

      return S : constant String :=
        Shader.Return_Value.To_String (Generator)
      do
         Generator.Pop_Environment;
      end return;

   end Compile_Evaluation;

   ---------------------
   -- Compile_Support --
   ---------------------

   procedure Compile_Support
     (Shader    : Root_Tau_Shader'Class;
      Generator : in out Tau.Generators.Root_Tau_Generator'Class)
   is
   begin
      for Argument of Shader.Arguments loop
         Argument.Compile (Generator);
      end loop;

      for Declaration of Shader.Declarations loop
         Declaration.Compile (Generator);
      end loop;

      for Statement of Shader.Statements loop
         Statement.Compile (Generator);
      end loop;

   end Compile_Support;

   ---------------
   -- To_Source --
   ---------------

   overriding function To_Source
     (Shader    : Root_Tau_Shader;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String
   is
      use type Tau.Environment.Tau_Environment;
      G : Tau.Generators.Root_Tau_Generator'Class renames
        Tau.Generators.Root_Tau_Generator'Class (Generator);
   begin
      if Shader.Bindings = null then
         raise Constraint_Error with
           "cannot convert " & Shader.Name & " to source because "
           & "it has no bindings";
      end if;

      return Shader.Compile_Evaluation (Shader.Bindings, G);
   end To_Source;

   --------------
   -- To_Value --
   --------------

   function To_Value
     (Shader : Tau_Shader)
      return Tau.Values.Tau_Value
   is
   begin
      return Shader.Value;
   end To_Value;

end Tau.Shaders;
