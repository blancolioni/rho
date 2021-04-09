with Rho.Nodes;

with Rho.Geometry;
with Rho.Material;
with Rho.Render;

package Rho.Meshes is

   type Root_Mesh_Type is
     new Rho.Nodes.Root_Node_Type with private;

   overriding procedure Load
     (Mesh       : in out Root_Mesh_Type;
      Target     : not null access Rho.Render.Render_Target'Class);

   overriding procedure Compile
     (Mesh       : in out Root_Mesh_Type;
      Target     : not null access Rho.Render.Render_Target'Class);

   overriding procedure Before_Render
     (Mesh       : in out Root_Mesh_Type;
      Target     : not null access Rho.Render.Render_Target'Class);

   overriding procedure Execute_Render
     (Mesh       : in out Root_Mesh_Type;
      Target     : not null access Rho.Render.Render_Target'Class);

   type Mesh_Type is access all Root_Mesh_Type'Class;

   function Create_Mesh
     (Geometry : Rho.Geometry.Geometry_Type;
      Material : Rho.Material.Material_Type)
      return Mesh_Type;

private

   type Root_Mesh_Type is
     new Rho.Nodes.Root_Node_Type with
      record
         Material : Rho.Material.Material_Type;
         Geometry : Rho.Geometry.Geometry_Type;
      end record;

   function Create_Mesh
     (Geometry : Rho.Geometry.Geometry_Type;
      Material : Rho.Material.Material_Type)
      return Mesh_Type
   is (new Root_Mesh_Type'
         (Rho.Nodes.Root_Node_Type with
            Material => Material,
            Geometry => Geometry));

end Rho.Meshes;
