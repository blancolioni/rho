private with Rho.Render;

with Rho.Matrices;
with Rho.Nodes;

package Rho.Cameras is

   type Root_Camera_Type (<>) is
     new Rho.Nodes.Root_Node_Type with private;

   procedure Set_Root_Object
     (Camera : Root_Camera_Type;
      Object : not null access Rho.Nodes.Root_Node_Type'Class);

   type Camera_Type is access all Root_Camera_Type'Class;

   function Perspective_Camera
     (Field_Of_View : Non_Negative_Real;
      Aspect_Ratio  : Non_Negative_Real;
      Near, Far     : Real)
      return Camera_Type;

   function Orthographic_Camera
     (Left, Bottom  : Real;
      Width, Height : Real)
      return Camera_Type;

private

   type Camera_Mode is (Orthographic, Perspective);

   type Root_Camera_Type (Mode : Camera_Mode) is
     new Rho.Nodes.Root_Node_Type with
      record
         Inverse_World_Matrix      : Rho.Matrices.Matrix_4;
         Projection_Matrix         : Rho.Matrices.Matrix_4;
         Inverse_Projection_Matrix : Rho.Matrices.Matrix_4;
         case Mode is
            when Orthographic =>
               Width, Height             : Real;
               Left, Bottom              : Real;
            when Perspective =>
               Field_Of_View             : Non_Negative_Real;
               Aspect_Ratio              : Non_Negative_Real;
               Near, Far                 : Real;
         end case;
      end record;

   overriding procedure Execute_Render
     (Camera : in out Root_Camera_Type;
      Target : not null access Rho.Render.Render_Target'Class);

   overriding procedure Update_Matrix
     (Camera : in out Root_Camera_Type);

   overriding function Class_Name
     (Camera : Root_Camera_Type)
      return String
   is ("camera");

   procedure Update_Projection_Matrix
     (Camera : in out Root_Camera_Type'Class);

end Rho.Cameras;
