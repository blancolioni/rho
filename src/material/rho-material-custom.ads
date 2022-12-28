private with Ada.Containers.Doubly_Linked_Lists;

with Rho.Shaders.Stages;

package Rho.Material.Custom is

   function Create
     (Shaders : Rho.Shaders.Stages.Shader_Array)
      return Rho.Material.Reference;

private

   subtype Parent is Rho.Material.Instance;
   subtype Dispatch is Parent'Class;

   package Shader_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Shaders.Stages.Shader_Type, Rho.Shaders.Stages."=");

   type Instance is new Parent with
      record
         Shaders : Shader_Lists.List;
      end record;

   subtype Any_Instance is Instance'Class;

   type Reference is access all Instance'Class;

   overriding procedure Load
     (Material : in out Instance;
      Target   : not null access Rho.Render.Render_Target'Class);

   overriding procedure Compile
     (Material : in out Instance;
      Target   : not null access Rho.Render.Render_Target'Class);

end Rho.Material.Custom;
