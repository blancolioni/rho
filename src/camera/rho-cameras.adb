with Rho.Elementary_Functions;

package body Rho.Cameras is

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Camera : in out Root_Camera_Type;
      Target : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Target.Set_Projection_Matrix (Camera.Projection_Matrix);
      Target.Set_Camera_Position (Camera.World_Position);
   end Execute_Render;

   -------------------------
   -- Orthographic_Camera --
   -------------------------

   function Orthographic_Camera
     (Left, Bottom  : Real;
      Width, Height : Non_Negative_Real)
      return Camera_Type
   is
   begin
      return Camera : constant Camera_Type :=
        new Root_Camera_Type'
          (Rho.Nodes.Root_Node_Type with
             Mode => Orthographic,
           Left => Left,
           Bottom => Bottom,
           Width  => Width,
           Height => Height,
           Inverse_World_Matrix      => <>,
           Projection_Matrix         => <>,
           Inverse_Projection_Matrix => <>)
      do
         Camera.Update_Projection_Matrix;
      end return;
   end Orthographic_Camera;

   ------------------------
   -- Perspective_Camera --
   ------------------------

   function Perspective_Camera
     (Field_Of_View : Non_Negative_Real; Aspect_Ratio : Non_Negative_Real;
      Near, Far     : Real) return Camera_Type
   is
   begin
      return Camera : constant Camera_Type :=
        new Root_Camera_Type'
          (Rho.Nodes.Root_Node_Type with
             Mode => Perspective,
           Field_Of_View             => Field_Of_View,
           Aspect_Ratio              => Aspect_Ratio,
           Near                      => Near,
           Far                       => Far,
           Inverse_World_Matrix      => <>,
           Projection_Matrix         => <>,
           Inverse_Projection_Matrix => <>)
      do
         Camera.Update_Projection_Matrix;
      end return;
   end Perspective_Camera;

   ---------------------
   -- Set_Root_Object --
   ---------------------

   procedure Set_Root_Object
     (Camera : Root_Camera_Type;
      Object : not null access Rho.Nodes.Root_Node_Type'Class)
   is
   begin
      Object.Set_World_Matrix (Camera.Inverse_World_Matrix);
   end Set_Root_Object;

   -------------------
   -- Update_Matrix --
   -------------------

   overriding procedure Update_Matrix
     (Camera : in out Root_Camera_Type)
   is
   begin
      Rho.Nodes.Root_Node_Type (Camera).Update_Matrix;
      Camera.Inverse_World_Matrix :=
        Rho.Matrices.Inverse (Camera.World_Matrix);
   end Update_Matrix;

   ------------------------------
   -- Update_Projection_Matrix --
   ------------------------------

   procedure Update_Projection_Matrix
     (Camera : in out Root_Camera_Type'Class)
   is
   begin
      case Camera.Mode is
         when Orthographic =>
            Camera.Projection_Matrix :=
              Rho.Matrices.Orthographic_Matrix
                (Left   => Camera.Left,
                 Right  => Camera.Left + Camera.Width,
                 Bottom => Camera.Bottom,
                 Top    => Camera.Bottom + Camera.Height);
         when Perspective =>
            declare
               use Rho.Elementary_Functions;
               Height : constant Non_Negative_Real :=
                          Camera.Near
                            * Tan (Camera.Field_Of_View / 2.0, 360.0);
               Width  : constant Non_Negative_Real :=
                          Height * Camera.Aspect_Ratio;
            begin
               Camera.Projection_Matrix :=
                 Rho.Matrices.Perspective_Matrix
                   (Left   => -Width,
                    Right  => Width,
                    Bottom => -Height,
                    Top    => Height,
                    Near   => Camera.Near,
                    Far    => Camera.Far);
            end;
      end case;
      Camera.Inverse_Projection_Matrix :=
        Rho.Matrices.Inverse (Camera.Projection_Matrix);
   end Update_Projection_Matrix;

end Rho.Cameras;
