with Rho.Color;
with Rho.Float_Arrays;
with Rho.Matrices;
with Rho.Rectangle;
with Rho.Render_Operation;
with Rho.Render_Target;
with Rho.Render_Window;
with Rho.Shader;
with Rho.Viewport;

package Rho.Rendering.GL_Renderer.GL_Window is

   type Rho_GL_Window_Record is
     new Rho.Render_Window.Rho_Render_Window_Record with private;

   overriding procedure Blend
     (Window : in out Rho_GL_Window_Record;
      Enabled : Boolean;
      Source_Function : Rho.Render_Target.Rho_Blend_Function :=
        Rho.Render_Target.One;
      Destination_Function : Rho.Render_Target.Rho_Blend_Function :=
        Rho.Render_Target.One);

   overriding procedure Set_Wireframe
     (Window : in out Rho_GL_Window_Record;
      Enabled : Boolean);

   overriding procedure Set_Back_Face_Removal
     (Window : in out Rho_GL_Window_Record;
      Enabled : Boolean);

   overriding procedure Set_Depth_Test
     (Window : in out Rho_GL_Window_Record;
      Enabled : Boolean);

   overriding procedure Enable_Point_Size
     (Window  : in out Rho_GL_Window_Record;
      Enabled : Boolean);

   overriding procedure Save_Matrix
     (Window : not null access Rho_GL_Window_Record;
      Matrix : in Rho.Matrices.Matrix_Mode_Type);

   overriding procedure Uniform_Float_Array
     (Window   : not null access Rho_GL_Window_Record;
      Uniform  : Rho.Shader.Rho_Uniform_Value;
      Value    : Rho.Float_Arrays.Real_Vector);

   type Rho_GL_Window is access all Rho_GL_Window_Record'Class;

   function Create_Top_Level_Window return Rho_GL_Window;

private

   type Rho_GL_Window_Record is
     new Rho.Render_Window.Rho_Render_Window_Record with
      record
         Rectangle              : Rho.Rectangle.Rho_Rectangle :=
                                    (100.0, 100.0, 640.0, 400.0);
         Window_Id              : Integer;
         Texture_Enabled        : Boolean := False;
         Lighting_Enabled       : Boolean := False;
         Current_Color          : Rho.Color.Rho_Color := (1.0, 1.0, 1.0, 1.0);
         Texture                : Rho.Texture.Rho_Texture := null;
         Current_Buffer         : Rho.Float_Buffer.Rho_Float_Buffer;
         Current_Vertex_Start   : Natural := 0;
         Current_Texture_Start  : Natural := 0;
         Current_Instance_Start : Natural := 0;
      end record;

   overriding procedure Set_Viewport
     (Window   : in out Rho_GL_Window_Record;
      Viewport : in Rho.Viewport.Rho_Viewport);

   overriding procedure Set_Clear_Color
     (Item  : in out Rho_GL_Window_Record;
      Color : Rho.Color.Rho_Color);

   overriding procedure Set_Rectangle
     (Item  : in out Rho_GL_Window_Record;
      Rectangle : Rho.Rectangle.Rho_Rectangle);

   overriding function Get_Rectangle
     (Item : Rho_GL_Window_Record)
      return Rho.Rectangle.Rho_Rectangle;

   overriding procedure Set_Full_Screen
     (Window      : in out Rho_GL_Window_Record;
      Full_Screen : Boolean);

   overriding procedure Before_Render
     (Window : not null access Rho_GL_Window_Record);

   overriding procedure After_Render
     (Window : not null access Rho_GL_Window_Record);

   overriding procedure Set_Output_Position
     (Target             : not null access Rho_GL_Window_Record;
      X, Y               : Rho_Float);

   overriding procedure Set_Raster_Position
     (Target             : not null access Rho_GL_Window_Record;
      Position : Rho.Matrices.Vector_3);

   overriding procedure Set_Texture
     (Target  : not null access Rho_GL_Window_Record;
      Texture : Rho.Texture.Rho_Texture);

   overriding procedure Draw_Buffer
     (Target          : not null access Rho_GL_Window_Record;
      Buffer          : Rho.Float_Buffer.Rho_Float_Buffer;
      Operation       : Rho.Render_Operation.Operation_Type;
      Count           : Natural;
      Instance_Count  : Positive := 1);

   overriding procedure Bind_Vertex_Attribute
     (Target          : not null access Rho_GL_Window_Record;
      Buffer          : Rho.Float_Buffer.Rho_Float_Buffer;
      Attribute       : Rho.Shader.Rho_Attribute_Value;
      Start           : Positive;
      Component_Size  : Positive;
      Is_Array        : Boolean);

end Rho.Rendering.GL_Renderer.GL_Window;
