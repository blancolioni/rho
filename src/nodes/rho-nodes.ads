private with Ada.Containers.Doubly_Linked_Lists;

with Tau.Shaders;

with Rho.Objects;
with Rho.Matrices;
with Rho.Renderable;
with Rho.Render;

package Rho.Nodes is

   type Node_Id is private;

   type Root_Node_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface
   with private;

   type Node_Type is access all Root_Node_Type'Class;

   function Parent (Node : Root_Node_Type'Class) return Node_Type;

   procedure Set_Position
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

   function Show (Node : Root_Node_Type) return String;

   type Shader_Component_Array is
     array (Positive range <>) of Tau.Shaders.Tau_Shader;

   function Shader_Components
     (Node : Root_Node_Type)
      return Shader_Component_Array;

   function Create_Node return Node_Type;

private

   type Node_Id is new Natural;

   package Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Node_Type);

   type Root_Node_Type is
     new Rho.Objects.Root_Object_Type
     and Rho.Renderable.Renderable_Interface with
      record
         Id                : Node_Id;
         Parent            : Node_Type;
         Children          : Node_Lists.List;
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

   procedure Invalidate_World_Matrix
     (Node : in out Root_Node_Type'Class);

   function Parent (Node : Root_Node_Type'Class) return Node_Type
   is (Node.Parent);

   No_Components : Shader_Component_Array (1 .. 0);

   function Shader_Components
     (Node : Root_Node_Type)
      return Shader_Component_Array
   is (No_Components);

end Rho.Nodes;
