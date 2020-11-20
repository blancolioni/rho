with Tau.Generators.Loader;

package body Rho.Material is

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Material   : in out Root_Material_Type;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Target.Activate_Shader (Material.Program);
   end Before_Render;

   ---------------------
   -- Create_Material --
   ---------------------

   function Create_Material
     (Name     : String;
      Bindings : Tau.Environment.Tau_Environment;
      Shaders  : Tau.Shaders.Lists.List)
      return Material_Type
   is
   begin
      return Result : constant Material_Type := new Root_Material_Type'
        (Rho.Objects.Root_Object_Type with
         Bindings => Bindings,
         Shaders => Shaders,
         Program => <>)
      do
         Result.Set_Name (Name);
      end return;
   end Create_Material;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Material   : in out Root_Material_Type;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Target.Bind_Shader (Material.Program);
   end Execute_Render;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Material : in out Root_Material_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
      Count   : constant Natural := Natural (Material.Shaders.Length);
      Index   : Positive := 1;
      Shaders : Rho.Shaders.Shader_Array (1 .. Count);
      Generator   : Tau.Generators.Root_Tau_Generator'Class :=
                      Tau.Generators.Loader.Load_Generator
                        (Target.Assets.Generator_Name);
   begin
      for Tau_Shader of Material.Shaders loop
         if not Tau_Shader.Compile (Material.Bindings, Generator) then
            raise Constraint_Error with
              "shader " & Tau_Shader.Name & " failed to compile";
         end if;
      end loop;

      Generator.Freeze;

      for Tau_Shader of Material.Shaders loop

         Shaders (Index) :=
           Rho.Shaders.Create
             (Name   => Tau_Shader.Name,
              Stage  => Tau_Shader.Stage,
              Source => Generator.Get_Source (Tau_Shader.Stage));

         if not Shaders (Index).Is_Loaded then
            Target.Assets.Compile_Shader (Shaders (Index));
         end if;

         Index := Index + 1;
      end loop;

      Material.Program :=
        Target.Assets.Create_Program
          (Name    => Material.Name,
           Shaders => Shaders);
      Material.Set_Loaded;
   end Load;

end Rho.Material;
