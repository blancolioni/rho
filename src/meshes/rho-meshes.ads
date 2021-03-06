private with Ada.Containers.Vectors;

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

   procedure Add_Material
     (Mesh     : in out Root_Mesh_Type;
      Material : Rho.Material.Material_Type);

   procedure Remove_Material
     (Mesh     : in out Root_Mesh_Type;
      Material : Rho.Material.Material_Type);

   type Mesh_Type is access all Root_Mesh_Type'Class;

   function Create_Mesh
     (Geometry : Rho.Geometry.Geometry_Type)
      return Mesh_Type;

   function Create_Mesh
     (Geometry : Rho.Geometry.Geometry_Type;
      Material : Rho.Material.Material_Type)
      return Mesh_Type;

private

   package Material_Vectors is
     new Ada.Containers.Vectors (Rho.Geometry.Material_Index,
                                 Rho.Material.Material_Type,
                                 Rho.Material."=");

   type Root_Mesh_Type is
     new Rho.Nodes.Root_Node_Type with
      record
         Material : Material_Vectors.Vector;
         Geometry : Rho.Geometry.Geometry_Type;
      end record;

end Rho.Meshes;
