with Rho.Shaders.Stages;

package body Rho.Material is

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
      for Stage in Shaders'Range loop
         declare
            Shader : Rho.Shaders.Stages.Shader_Type renames
                       Shaders (Stage);
         begin
            Shader :=
              Rho.Shaders.Stages.Create
                (Name  => Material.Name & "-" & Stage_Name (Stage),
                 Stage => Stage);
            Shader.Add_Slices (Target.Active_Shader_Slices);
            Shader.Add_Slices
              (Root_Material_Type'Class (Material).Shader_Slices);
            Shader.Build;
            Target.Compile_Shader (Shader);
         end;
      end loop;

      Material.Program :=
        Target.Create_Program
          (Name    => Material.Name,
           Shaders => (Shaders (Vertex_Shader), Shaders (Fragment_Shader)));

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
      for Texture of Material.Textures loop
         Texture.Load (Target);
         Rho.Shaders.Slices.Add_Slices (Material, Texture);
      end loop;
   end Load;

end Rho.Material;
