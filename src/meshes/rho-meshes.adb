package body Rho.Meshes is

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Mesh   : in out Root_Mesh_Type;
      Target :        not null access Rho.Render.Render_Target'Class)
   is
   begin
      Rho.Nodes.Root_Node_Type (Mesh).Before_Render (Target);
      Mesh.Geometry.Before_Render (Target);
      Mesh.Material.Before_Render (Target);
   end Before_Render;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Mesh       : in out Root_Mesh_Type;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
   begin

      Rho.Nodes.Root_Node_Type (Mesh).Compile (Target);

      Mesh.Material.Compile (Target);
   end Compile;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Mesh   : in out Root_Mesh_Type;
      Target :        not null access Rho.Render.Render_Target'Class)
   is
   begin
      Rho.Nodes.Root_Node_Type (Mesh).Execute_Render (Target);
      Mesh.Geometry.Execute_Render (Target);
      Mesh.Material.Execute_Render (Target);
      Target.Render_Current_Buffers;
   end Execute_Render;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Mesh       : in out Root_Mesh_Type;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
   begin

      Rho.Nodes.Root_Node_Type (Mesh).Load (Target);

      if not Mesh.Geometry.Is_Loaded then
         Mesh.Geometry.Load (Target);
      end if;

      if not Mesh.Material.Is_Loaded then
         Mesh.Material.Load (Target);
      end if;

   end Load;

end Rho.Meshes;
