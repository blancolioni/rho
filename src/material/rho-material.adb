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
   begin
      for Shader of Material.Shaders loop
         Shaders (Index) := Target.Assets.Shader (Shader, Material.Bindings);
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
