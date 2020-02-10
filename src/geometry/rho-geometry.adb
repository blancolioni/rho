package body Rho.Geometry is

   ---------------------
   -- Create_Geometry --
   ---------------------

   function Create_Geometry return Geometry_Type is
   begin
      return new Root_Geometry_Type'
        (Rho.Objects.Root_Object_Type with
         Vertices => Rho.Buffers.Create_Buffer (Rho.Buffers.Vector_3_Data),
         Normals  => Rho.Buffers.Create_Buffer (Rho.Buffers.Vector_3_Data),
         UVs      => Rho.Buffers.Create_Buffer (Rho.Buffers.Vector_2_Data),
         Faces    => Rho.Buffers.Create_Buffer (Rho.Buffers.Integer_Data));
   end Create_Geometry;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Geometry : in out Root_Geometry_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Target.Activate_Buffer
        (Geometry.Vertices, Target.Current_Shader.Vertex_Position_Attribute);
      Target.Activate_Buffer
        (Geometry.Faces, Target.Current_Shader.Vertex_Position_Attribute);
   end Execute_Render;

   ----------
   -- Face --
   ----------

   procedure Face
     (Geometry : in out Root_Geometry_Type'Class;
      A, B, C  : Vertex_Index)
   is
   begin
      Geometry.Face ((A, B, C));
   end Face;

   ----------
   -- Face --
   ----------

   procedure Face
     (Geometry : in out Root_Geometry_Type'Class;
      Vertices :        Vertex_Index_Array)
   is
   begin
      for Index of Vertices loop
         Geometry.Faces.Append (Natural (Index) - 1);
      end loop;
   end Face;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Geometry : in out Root_Geometry_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Target.Load_Buffer (Geometry.Vertices);
      Target.Load_Buffer (Geometry.Normals);
      Target.Load_Buffer (Geometry.UVs);
      Target.Load_Buffer (Geometry.Faces);
      Geometry.Set_Loaded;
   end Load;

   ------------
   -- Normal --
   ------------

   procedure Normal
     (Geometry : in out Root_Geometry_Type'Class;
      Vector   :        Rho.Matrices.Normal_Vector_3)
   is
   begin
      Geometry.Normals.Append (Vector);
   end Normal;

   ------------
   -- Normal --
   ------------

   procedure Normal
     (Geometry : in out Root_Geometry_Type'Class;
      X, Y, Z  : Signed_Unit_Real)
   is
   begin
      Geometry.Normals.Append (Rho.Matrices.To_Vector (X, Y, Z));
   end Normal;

   -------------
   -- Texture --
   -------------

   procedure Texture
     (Geometry : in out Root_Geometry_Type'Class;
      U, V     : Unit_Real)
   is
   begin
      Geometry.UVs.Append (Rho.Matrices.To_Vector (U, V));
   end Texture;

   ------------
   -- Vertex --
   ------------

   procedure Vertex
     (Geometry : in out Root_Geometry_Type'Class;
      Vector   :        Rho.Matrices.Vector_3)
   is
   begin
      Geometry.Vertices.Append (Vector);
   end Vertex;

   ------------
   -- Vertex --
   ------------

   procedure Vertex
     (Geometry : in out Root_Geometry_Type'Class;
      X, Y, Z  : Real)
   is
   begin
      Geometry.Vertices.Append (Rho.Matrices.To_Vector (X, Y, Z));
   end Vertex;

end Rho.Geometry;
