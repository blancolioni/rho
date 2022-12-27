with Rho.Shaders.Variables;

package body Rho.Geometry is

   -----------------
   -- Begin_Group --
   -----------------

   procedure Begin_Group
     (Geometry : in out Root_Geometry_Type'Class;
      Material : Material_Index)
   is
   begin
      Geometry.Groups.Append
        (Group_Record'
           (Faces    => Rho.Buffers.Create_Buffer (Rho.Buffers.Integer_Data),
            Material => Material));
   end Begin_Group;

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
         Faces    => <>,
         Groups   => <>);
   end Create_Geometry;

   ---------------
   -- End_Group --
   ---------------

   procedure End_Group
     (Geometry : in out Root_Geometry_Type'Class)
   is
   begin
      null;
   end End_Group;

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
      if Geometry.Groups.Is_Empty then
         Geometry.Begin_Group (1);
      end if;

      declare
         Faces : Rho.Buffers.Buffer_Type renames
                   Geometry.Groups.Last_Element.Faces;
      begin
         for Vertex of Vertices loop
            Faces.Append (Natural (Vertex) - 1);
         end loop;
      end;
   end Face;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Geometry : in out Root_Geometry_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
   begin
      if Geometry.Is_Loaded then
         return;
      end if;

      Target.Load_Buffer (Geometry.Vertices);
      Target.Load_Buffer (Geometry.Normals);
      Target.Load_Buffer (Geometry.UVs);

      for Group of Geometry.Groups loop
         Target.Load_Buffer (Group.Faces);
      end loop;

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

   ------------
   -- Render --
   ------------

   procedure Render
     (Geometry          : Root_Geometry_Type'Class;
      Target            : not null access Rho.Render.Render_Target'Class;
      Activate_Material : not null access
        procedure (Index : Material_Index);
      Render_Material   : not null access
        procedure (Index : Material_Index))
   is
      use Rho.Shaders;

      function Attribute
        (Binding : Standard_Attribute_Binding)
         return Rho.Shaders.Variables.Variable_Type
      is (Target.Current_Shader.Standard_Binding (Binding));

   begin
      for Group of Geometry.Groups loop
         Activate_Material (Group.Material);

         Target.Activate_Buffer
           (Geometry.Vertices,
            Attribute (Position_Attribute));

         Target.Activate_Buffer
           (Geometry.Normals,
            Attribute (Vertex_Normal_Attribute));

         Target.Activate_Buffer
           (Geometry.UVs,
            Attribute (Vertex_Texture_Coord_Attribute));

         Target.Activate_Buffer
           (Buffer   => Group.Faces,
            Argument => Attribute (Position_Attribute));

         Render_Material (Group.Material);
         Target.Render_Current_Buffers;
      end loop;

   end Render;

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
