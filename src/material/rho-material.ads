with Tau.Environment;
with Tau.Shaders.Lists;

with Rho.Objects;
with Rho.Render;
with Rho.Renderable;
with Rho.Shaders;

package Rho.Material is

   type Root_Material_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface
   with private;

   type Material_Type is access all Root_Material_Type'Class;

   function Create_Material
     (Name     : String;
      Bindings : Tau.Environment.Tau_Environment;
      Shaders  : Tau.Shaders.Lists.List)
      return Material_Type;

private

   type Root_Material_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface with
      record
         Bindings : Tau.Environment.Tau_Environment;
         Shaders  : Tau.Shaders.Lists.List;
         Program  : Rho.Shaders.Program_Type;
      end record;

   overriding procedure Load
     (Material : in out Root_Material_Type;
      Target   : not null access Rho.Render.Render_Target'Class);

   overriding procedure Before_Render
     (Material   : in out Root_Material_Type;
      Target     : not null access Rho.Render.Render_Target'Class);

   overriding procedure Execute_Render
     (Material   : in out Root_Material_Type;
      Target     : not null access Rho.Render.Render_Target'Class);

   overriding function Class_Name
     (Material : Root_Material_Type)
      return String
   is ("material");

end Rho.Material;
