with Ada.Text_IO;

with Rho.Shaders.Variables;
with Rho.Logging;

package body Rho.Material.Custom is

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Material : in out Instance;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
      procedure Bind
        (Binding : Rho.Shaders.Standard_Variable_Binding;
         Count   : Positive := 1);

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Binding : Rho.Shaders.Standard_Variable_Binding;
         Count   : Positive := 1)
      is
         V : constant Rho.Shaders.Variables.Variable_Type :=
               Rho.Shaders.Variables.New_Binding
                 (Rho.Shaders.Standard_Binding_Name (Binding),
                  Binding,
                  Count);
      begin
         Rho.Logging.Log ("binding: " & V.Name);
         Material.Program.Add_Variable (V);
         Target.Bind_Variable (Material.Program, V);
      end Bind;

   begin
      if Material.Compiled then
         return;
      end if;

      Ada.Text_IO.Put_Line ("compiling: " & Material.Name);

      for Shader of Material.Shaders loop
         Target.Compile_Shader (Shader);
      end loop;
      Material.Program :=
        Target.Create_Program
          (Name    => Material.Name,
           Shaders =>
             (Material.Shaders.First_Element,
              Material.Shaders.Last_Element));

      Bind (Rho.Shaders.Model_Uniform);
      Bind (Rho.Shaders.View_Uniform);
      Bind (Rho.Shaders.Position_Attribute, 3);

      Material.Compiled := True;

   end Compile;

   ----------------------------
   -- Create_Custom_Material --
   ----------------------------

   function Create
     (Shaders : Rho.Shaders.Stages.Shader_Array)
      return Rho.Material.Reference
   is
      Result : constant Reference := new Instance;
   begin
      for Shader of Shaders loop
         Result.Shaders.Append (Shader);
      end loop;
      return Rho.Material.Reference (Result);
   end Create;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Material : in out Instance;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
   begin
      if Material.Loaded then
         return;
      end if;

      Material.Loaded := True;

   end Load;

end Rho.Material.Custom;
