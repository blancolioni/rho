with Rho.Shaders.Stages;
with Tau.Generators;

package body Rho.Material is

   ----------------
   -- Add_Shader --
   ----------------

   procedure Add_Shader
     (Material : in out Root_Material_Type'Class;
      Shader   : Tau.Shaders.Tau_Shader)
   is
   begin
      Material.Compiler.Add_Shader (Shader);
   end Add_Shader;

   ----------------
   -- Add_Shader --
   ----------------

   procedure Add_Shader
     (Material    : in out Root_Material_Type'Class;
      Shader_Name : String)
   is
   begin
      if not Material.Shader_Names.Contains (Shader_Name) then
         Material.Shader_Names.Append (Shader_Name);
      end if;
   end Add_Shader;

   ---------------
   -- Add_Slice --
   ---------------

   overriding procedure Add_Slice
     (Material : in out Root_Material_Type;
      Slice    : Rho.Shaders.Slices.Slice_Type)
   is
   begin
      Material.Slices.Add_Slice (Slice);
   end Add_Slice;

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Material   : in out Root_Material_Type;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Target.Activate_Shader (Material.Program);
      for Texture of Material.Textures loop
         Texture.Activate (Target);
      end loop;
   end Before_Render;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Material : in out Root_Material_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
      Shaders : array (Shader_Stage) of Rho.Shaders.Stages.Shader_Type;
   begin
      if Material.Compiled then
         return;
      end if;

      for Shader of Target.Active_Shaders loop
         Material.Compiler.Add_Shader (Shader);
      end loop;

      Material.Compiler.Link;

      declare
         Gen : Tau.Generators.Root_Tau_Generator'Class := Target.Generator;
      begin
         Gen.Start (Material.Name);
         Material.Compiler.Generate (Gen);

         for Stage in Shader_Stage loop
            declare
               Shader : Rho.Shaders.Stages.Shader_Type renames
                          Shaders (Stage);
            begin
               Shader :=
                 Rho.Shaders.Stages.Create
                   (Name  => Material.Name & "-" & Stage_Name (Stage),
                    Stage => Stage,
                    Source => Gen.Shader_Source (Stage));
               Target.Compile_Shader (Shader);
            end;
         end loop;
      end;

      Material.Program :=
        Target.Create_Program
          (Name    => Material.Name,
           Shaders => (Shaders (Vertex_Shader), Shaders (Fragment_Shader)));

      Material.Compiled := True;

   end Compile;

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
   begin
      if Material.Loaded then
         return;
      end if;

      Material.Compiler.Add_Shader
        (Target.Assets.Shader ("rho-shaders-material"));

      for Shader_Name of Material.Shader_Names loop
         Material.Compiler.Add_Shader
           (Target.Assets.Shader (Shader_Name));
      end loop;

      for Texture of Material.Textures loop
         Texture.Load (Target);
         Rho.Shaders.Slices.Add_Slices (Material, Texture);
      end loop;

      Material.Loaded := True;

   end Load;

end Rho.Material;
