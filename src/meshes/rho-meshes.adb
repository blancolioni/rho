package body Rho.Meshes is

   ------------------
   -- Add_Material --
   ------------------

   procedure Add_Material
     (Mesh     : in out Root_Mesh_Type;
      Material : Rho.Material.Reference)
   is
   begin
      Mesh.Material.Append (Material);
   end Add_Material;

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

      for Material of Mesh.Material loop
         --  Material.Add_Shader
         --    (Target.Assets.Shader ("node"));
         Material.Compile (Target);
      end loop;
   end Compile;

   -----------------
   -- Create_Mesh --
   -----------------

   function Create_Mesh
     (Geometry : Rho.Geometry.Geometry_Type)
      return Mesh_Type
   is
   begin
      return new Root_Mesh_Type'
        (Rho.Nodes.Root_Node_Type with
           Material => <>,
         Geometry => Geometry);
   end Create_Mesh;

   -----------------
   -- Create_Mesh --
   -----------------

   function Create_Mesh
     (Geometry : Rho.Geometry.Geometry_Type;
      Material : Rho.Material.Reference)
      return Mesh_Type
   is
   begin
      return Mesh : constant Mesh_Type := Create_Mesh (Geometry) do
         Mesh.Add_Material (Material);
      end return;
   end Create_Mesh;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Mesh   : in out Root_Mesh_Type;
      Target :        not null access Rho.Render.Render_Target'Class)
   is
      procedure Activate_Material (Index : Rho.Geometry.Material_Index);
      procedure Render_Material (Index : Rho.Geometry.Material_Index);

      function Get_Material
        (Index : Rho.Geometry.Material_Index)
         return Rho.Material.Reference;

      -----------------------
      -- Activate_Material --
      -----------------------

      procedure Activate_Material (Index : Rho.Geometry.Material_Index) is
      begin
         Get_Material (Index).Before_Render (Target);
      end Activate_Material;

      ------------------
      -- Get_Material --
      ------------------

      function Get_Material
        (Index : Rho.Geometry.Material_Index)
         return Rho.Material.Reference
      is
         use Rho.Geometry;
         Real_Index : constant Material_Index :=
                        Material_Index ((Natural (Index) - 1)
                                        mod Natural (Mesh.Material.Last_Index)
                                        + 1);
         Material   : constant Rho.Material.Reference :=
                        Mesh.Material.Element (Real_Index);
      begin
         return Material;
      end Get_Material;

      ---------------------
      -- Render_Material --
      ---------------------

      procedure Render_Material (Index : Rho.Geometry.Material_Index) is
      begin
         Get_Material (Index).Execute_Render (Target);
      end Render_Material;

   begin
      Rho.Nodes.Root_Node_Type (Mesh).Execute_Render (Target);
      Mesh.Geometry.Render
        (Target            => Target,
         Activate_Material => Activate_Material'Access,
         Render_Material   => Render_Material'Access);
   end Execute_Render;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This     : in out Root_Mesh_Type;
      Geometry : Rho.Geometry.Geometry_Type;
      Material : Rho.Material.Reference)
   is
   begin
      This.Geometry := Geometry;
      This.Add_Material (Material);
   end Initialize;

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

      for Material of Mesh.Material loop
         if not Material.Is_Loaded then
            Material.Load (Target);
         end if;
      end loop;

   end Load;

   ---------------------
   -- Remove_Material --
   ---------------------

   procedure Remove_Material
     (Mesh     : in out Root_Mesh_Type;
      Material : Rho.Material.Reference)
   is
      use Material_Vectors;
      Position : Cursor := Mesh.Material.Find (Material);
   begin
      if Has_Element (Position) then
         Mesh.Material.Delete (Position);
      end if;
   end Remove_Material;

   ------------------
   -- Set_Geometry --
   ------------------

   procedure Set_Geometry
     (Mesh     : in out Root_Mesh_Type;
      Geometry : Rho.Geometry.Geometry_Type)
   is
   begin
      Mesh.Geometry := Geometry;
   end Set_Geometry;

end Rho.Meshes;
