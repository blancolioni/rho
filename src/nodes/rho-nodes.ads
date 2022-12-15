private with Ada.Containers.Doubly_Linked_Lists;

with Rho.Objects;
with Rho.Matrices;
with Rho.Renderable;
with Rho.Render;

with Rho.Shaders.Slices;

package Rho.Nodes is

   type Root_Node_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface
     and Rho.Shaders.Slices.Slice_Container_Interface
   with private;

   type Node_Type is access all Root_Node_Type'Class;

   function Parent (Node : Root_Node_Type'Class) return Node_Type;

   function Position
     (Node : Root_Node_Type'Class)
      return Rho.Matrices.Vector_3;

   function World_Position
     (Node : in out Root_Node_Type'Class)
      return Rho.Matrices.Vector_3;

   procedure Set_Position
     (Node  : in out Root_Node_Type'Class;
      X, Y, Z : Real);

   procedure Scale
     (Node  : in out Root_Node_Type'Class;
      X, Y, Z : Real);

   procedure Rotate_On_Axis
     (Node  : in out Root_Node_Type'Class;
      Axis  : Rho.Matrices.Normal_Vector_3;
      Angle : Real);

   procedure Rotate_X
     (Node  : in out Root_Node_Type'Class;
      Angle : Real);

   procedure Rotate_Y
     (Node  : in out Root_Node_Type'Class;
      Angle : Real);

   procedure Rotate_Z
     (Node  : in out Root_Node_Type'Class;
      Angle : Real);

   function Local_Matrix
     (Node : in out Root_Node_Type'Class)
      return Rho.Matrices.Matrix_4;

   function World_Matrix
     (Node : in out Root_Node_Type'Class)
      return Rho.Matrices.Matrix_4;

   procedure Set_World_Matrix
     (Node   : in out Root_Node_Type'Class;
      Matrix : Rho.Matrices.Matrix_4)
     with Pre => Node.Parent = null;

   procedure Update_Matrix
     (Node : in out Root_Node_Type);

   procedure Update_World_Matrix
     (Node : in out Root_Node_Type);

   procedure On_Quaternion_Changed
     (Node : in out Root_Node_Type);

   procedure Add
     (Node : not null access Root_Node_Type;
      Child  : not null access Root_Node_Type'Class);

   procedure Remove
     (Node  : not null access Root_Node_Type;
      Child : not null access Root_Node_Type'Class);

   procedure Iterate
     (Root : in out Root_Node_Type'Class;
      Process : not null access
        procedure (Node : in out Root_Node_Type'Class));

   procedure Iterate_Children
     (Root : in out Root_Node_Type'Class;
      Process : not null access
        procedure (Node : not null access Root_Node_Type'Class));

   function Show (Node : Root_Node_Type) return String;

   overriding function Shader_Slices
     (Node : Root_Node_Type)
      return Rho.Shaders.Slices.Slice_Array;

   overriding procedure Add_Slice
     (Node  : in out Root_Node_Type;
      Slice : Rho.Shaders.Slices.Slice_Type);

   function Create_Node return Node_Type;

private

   package Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Node_Type);

   type Root_Node_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface
       and Rho.Shaders.Slices.Slice_Container_Interface with
      record
         Parent            : Node_Type;
         Children          : Node_Lists.List;
         Slices            : Rho.Shaders.Slices.Slice_Container;
         Position          : Rho.Matrices.Vector_3;
         Quaternion        : Rho.Matrices.Quaternion;
         Scale             : Rho.Matrices.Vector_3 :=
           Rho.Matrices.To_Vector (1.0, 1.0, 1.0);
         M_Local           : Rho.Matrices.Matrix_4;
         M_World           : Rho.Matrices.Matrix_4;
         Local_Out_Of_Date : Boolean := False;
         World_Out_Of_Date : Boolean := False;
         Is_Visible        : Boolean := True;
      end record;

   overriding procedure Load
     (Node     : in out Root_Node_Type;
      Target     : not null access Rho.Render.Render_Target'Class);

   overriding procedure Before_Render
     (Node   : in out Root_Node_Type;
      Target : not null access Rho.Render.Render_Target'Class);

   overriding procedure After_Render
     (Node   : in out Root_Node_Type;
      Target : not null access Rho.Render.Render_Target'Class);

   overriding procedure Execute_Render
     (Node   : in out Root_Node_Type;
      Target : not null access Rho.Render.Render_Target'Class);

   overriding function Class_Name
     (Node : Root_Node_Type)
      return String
   is ("node");

   function Position
     (Node : Root_Node_Type'Class)
      return Rho.Matrices.Vector_3
   is (Node.Position);

   procedure Invalidate_World_Matrix
     (Node : in out Root_Node_Type'Class);

   function Parent (Node : Root_Node_Type'Class) return Node_Type
   is (Node.Parent);

   overriding function Shader_Slices
     (Node : Root_Node_Type)
      return Rho.Shaders.Slices.Slice_Array
   is (Node.Slices.Shader_Slices);

end Rho.Nodes;
