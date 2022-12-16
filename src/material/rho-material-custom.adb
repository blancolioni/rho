with Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;

package body Rho.Material.Custom is

   package Shader_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Shaders.Stages.Shader_Type, Rho.Shaders.Stages."=");

   type Custom_Material_Type is
     new Root_Material_Type with
      record
         Shaders : Shader_Lists.List;
      end record;

   type Custom_Material_Access is
     access all Custom_Material_Type'Class;

   overriding procedure Load
     (Material : in out Custom_Material_Type;
      Target   : not null access Rho.Render.Render_Target'Class);

   overriding procedure Compile
     (Material : in out Custom_Material_Type;
      Target   : not null access Rho.Render.Render_Target'Class);

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Material : in out Custom_Material_Type;
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

   function Create_Custom_Material
     (Shaders : Rho.Shaders.Stages.Shader_Array) return Material_Type
   is
      Result : constant Custom_Material_Access := new Custom_Material_Type;
   begin
      for Shader of Shaders loop
         Result.Shaders.Append (Shader);
      end loop;
      return Material_Type (Result);
   end Create_Custom_Material;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Material : in out Custom_Material_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
   begin
      if Material.Loaded then
         return;
      end if;

      Material.Loaded := True;

   end Load;

end Rho.Material.Custom;
