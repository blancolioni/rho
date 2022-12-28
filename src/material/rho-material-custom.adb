with Ada.Text_IO;

package body Rho.Material.Custom is

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Material : in out Instance;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
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
