with Ada.Text_IO;

with Rho.Logging;
with Rho.Main;

with Rho.Keyboard;

with GLUT;

with GL;
with GL_Constants;                  use GL_Constants;
with GL_Types;                      use GL_Types;

package body Rho.Rendering.GL_Renderer.GL_Window is

   Log_Operations : constant Boolean := False;

   Local_Top_Window : Rho_GL_Window;

   procedure Display_Handler;

   procedure Idle_Handler;

   procedure Reshape_Handler
     (Width, Height : Integer);

   procedure Mouse_Button_Handler
     (GL_Button : Integer;
      GL_State  : Integer;
      X, Y      : Integer);

   procedure Mouse_Move_Handler
     (X, Y   : Integer);

   procedure Key_Down_Handler
     (Key : GLUT.Key_Type;
      X, Y : Integer);

   procedure Key_Up_Handler
     (Key : GLUT.Key_Type;
      X, Y : Integer);

   procedure Special_Key_Handler
     (Key : Integer;
      X, Y : Integer);

   ------------------
   -- After_Render --
   ------------------

   overriding procedure After_Render
     (Window : not null access Rho_GL_Window_Record)
   is
      pragma Unreferenced (Window);
   begin
      null;
   end After_Render;

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Window : not null access Rho_GL_Window_Record)
   is
      pragma Unreferenced (Window);
   begin
      GL.Clear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
   end Before_Render;

   ---------------------------
   -- Bind_Vertex_Attribute --
   ---------------------------

   overriding procedure Bind_Vertex_Attribute
     (Target          : not null access Rho_GL_Window_Record;
      Buffer          : Rho.Float_Buffer.Rho_Float_Buffer;
      Attribute       : Rho.Shader.Rho_Attribute_Value;
      Start           : Positive;
      Component_Size  : Positive;
      Is_Array        : Boolean)
   is
      use type Rho.Float_Buffer.Rho_Float_Buffer;
   begin
      if Target.Current_Buffer /= Buffer then
         Target.Current_Buffer := Buffer;
         GL.Bind_Buffer (GL_ARRAY_BUFFER, Uint (Buffer.Id));
         Target.Current_Vertex_Start := 0;
         Target.Current_Texture_Start := 0;
         Target.Current_Instance_Start := 0;
      end if;

      GL.Vertex_Attribute_Pointer
        (Index        => Uint (Attribute.Location),
         Size         => Int (Component_Size),
         Element_Type => GL_FLOAT,
         Normalized   => GL_FALSE,
         Stride       => 0,
         Pointer      => Buffer.To_Offset (Start));
      GL.Enable_Vertex_Attribute_Array (Uint (Attribute.Location));

      if Is_Array then
         GL.Vertex_Attribute_Divisor (Uint (Attribute.Location), 1);
      end if;

   end Bind_Vertex_Attribute;

   -----------
   -- Blend --
   -----------

   overriding procedure Blend
     (Window               : in out Rho_GL_Window_Record;
      Enabled              : Boolean;
      Source_Function      : Rho.Render_Target.Rho_Blend_Function :=
        Rho.Render_Target.One;
      Destination_Function : Rho.Render_Target.Rho_Blend_Function :=
        Rho.Render_Target.One)
   is
      pragma Unreferenced (Window);

      use all type Rho.Render_Target.Rho_Blend_Function;

      function GL_Blend_Function
        (Fn : Rho.Render_Target.Rho_Blend_Function)
         return GLenum
      is ((case Fn is
              when Zero => 0,
              when One  => 1,
              when Source_Alpha => GL_SRC_ALPHA,
              when Destination_Alpha => GL_DST_ALPHA,
              when One_Minus_Source_Alpha => GL_ONE_MINUS_SRC_ALPHA,
              when One_Minus_Destination_Alpha =>
                 GL_ONE_MINUS_DST_ALPHA));
   begin
      if Enabled then
         GL.Enable (GL_BLEND);
         GL.Blend_Func (GL_Blend_Function (Source_Function),
                        GL_Blend_Function (Destination_Function));
      else
         GL.Disable (GL_BLEND);
      end if;
   end Blend;

   -----------------------------
   -- Create_Top_Level_Window --
   -----------------------------

   function Create_Top_Level_Window return Rho_GL_Window is
      Result : constant Rho_GL_Window :=
                 new Rho_GL_Window_Record;
   begin

      GLUT.Init_Window_Size (Integer (Result.Width), Integer (Result.Height));
      GLUT.Init_Window_Position (Integer (Result.X), Integer (Result.Y));

      Result.Window_Id := GLUT.Create_Window ("Maas GL");

      GLUT.Display_Function (Display_Handler'Access);
      GLUT.Reshape_Function (Reshape_Handler'Access);
      GLUT.Mouse_Function (Mouse_Button_Handler'Access);
      GLUT.Motion_Function (Mouse_Move_Handler'Access);
      GLUT.Passive_Motion_Function (Mouse_Move_Handler'Access);
      GLUT.Keyboard_Function (Key_Down_Handler'Access);
      GLUT.Keyboard_Up_Function (Key_Up_Handler'Access);
      GLUT.Special_Function (Special_Key_Handler'Access);

      GLUT.Idle_Function (Idle_Handler'Access);

      Local_Top_Window := Result;

      return Result;

   end Create_Top_Level_Window;

   Frame_Count : Natural := 0;

   ---------------------
   -- Display_Handler --
   ---------------------

   procedure Display_Handler is
   begin
      Frame_Count := Frame_Count + 1;
      Rho.Main.Render_One_Frame;
      GLUT.Swap_Buffers;
      if GL.Debug_Enabled then
         Ada.Text_IO.Put_Line ("------------------------------------");
         if Frame_Count > 2 then
            GL.Disable_Debug;
         end if;
      end if;
   end Display_Handler;

   -----------------
   -- Draw_Buffer --
   -----------------

   overriding procedure Draw_Buffer
     (Target          : not null access Rho_GL_Window_Record;
      Buffer          : Rho.Float_Buffer.Rho_Float_Buffer;
      Operation       : Rho.Render_Operation.Operation_Type;
      Count           : Natural;
      Instance_Count  : Positive := 1)
   is

--        pragma Unreferenced (Target);
--        pragma Unreferenced (Buffer);

      use type Rho.Float_Buffer.Rho_Float_Buffer;

      use Rho.Render_Operation;

      To_GL_Operation : constant array (Operation_Type) of GLenum :=
                          (Point_List     => GL_POINTS,
                           Line_List      => GL_LINES,
                           Line_Strip     => GL_LINE_STRIP,
                           Triangle_List  => GL_TRIANGLES,
                           Triangle_Strip => GL_TRIANGLE_STRIP,
                           Triangle_Fan   => GL_TRIANGLE_FAN,
                           Quad_List      => GL_QUADS);
   begin

      if Buffer /= Target.Current_Buffer then
         Target.Current_Buffer := Buffer;
      end if;

      if Instance_Count = 1 then
         GL.Draw_Arrays
           (Mode  => To_GL_Operation (Operation),
            First => 0,
            Count => Sizei (Count));
      else
         GL.Draw_Arrays_Instanced
           (Mode            => To_GL_Operation (Operation),
            First           => 0,
            Count           => Sizei (Count),
            Primitive_Count => Sizei (Instance_Count));
      end if;

   end Draw_Buffer;

   -----------------------
   -- Enable_Point_Size --
   -----------------------

   overriding procedure Enable_Point_Size
     (Window  : in out Rho_GL_Window_Record;
      Enabled : Boolean)
   is
      pragma Unreferenced (Window);
   begin
      if Enabled then
         GL.Enable (GL_VERTEX_PROGRAM_POINT_SIZE_ARB);
      else
         GL.Disable (GL_VERTEX_PROGRAM_POINT_SIZE_ARB);
      end if;
   end Enable_Point_Size;

   -------------------
   -- Get_Rectangle --
   -------------------

   overriding function Get_Rectangle
     (Item : Rho_GL_Window_Record)
      return Rho.Rectangle.Rho_Rectangle
   is
   begin
      return Item.Rectangle;
   end Get_Rectangle;

   ------------------
   -- Idle_Handler --
   ------------------

   procedure Idle_Handler is
   begin
      GLUT.Post_Redisplay;
   end Idle_Handler;

   -----------------
   -- Key_Handler --
   -----------------

   procedure Key_Down_Handler
     (Key : GLUT.Key_Type;
      X, Y : Integer)
   is
   begin
      if Natural (Key) = 13 then
         Rho.Keyboard.Key_Down (Character'Val (10));
      else
         Rho.Keyboard.Key_Down (Character'Val (Key));
      end if;
      Mouse_Move_Handler (X, Y);
   end Key_Down_Handler;

   --------------------
   -- Key_Up_Handler --
   --------------------

   procedure Key_Up_Handler
     (Key : GLUT.Key_Type;
      X, Y : Integer)
   is
   begin
      if Natural (Key) = 13 then
         Rho.Keyboard.Key_Up (Character'Val (10));
      else
         Rho.Keyboard.Key_Up (Character'Val (Key));
      end if;
      Mouse_Move_Handler (X, Y);
   end Key_Up_Handler;

   --------------------------
   -- Mouse_Button_Handler --
   --------------------------

   procedure Mouse_Button_Handler
     (GL_Button : Integer;
      GL_State  : Integer;
      X, Y   : Integer)
   is
      Button : Rho.Mouse.Mouse_Button;
      State  : Rho.Mouse.Button_State;
   begin
      pragma Assert (GL_Button in
                       GLUT.Mouse_Buttons | GLUT.Mouse_Wheel_Buttons);
      pragma Assert (GL_State in GLUT.DOWN | GLUT.UP);

      if GL_Button in GLUT.Mouse_Buttons then
         case GLUT.Mouse_Buttons (GL_Button) is
            when GLUT.LEFT_BUTTON =>
               Button := Rho.Mouse.Left;
            when GLUT.MIDDLE_BUTTON =>
               Button := Rho.Mouse.Middle;
            when GLUT.RIGHT_BUTTON =>
               Button := Rho.Mouse.Right;
         end case;

         if GL_State = GLUT.DOWN then
            State := Rho.Mouse.Down;
         else
            State := Rho.Mouse.Up;
         end if;

         GL_Mouse.State.Button (Button) := State;

      else
         if GL_State = GLUT.DOWN then
            case GLUT.Mouse_Wheel_Buttons (GL_Button) is
               when GLUT.SCROLL_UP_BUTTON =>
                  GL_Mouse.State.Wheel := Rho.Mouse.Move_Up;
               when GLUT.SCROLL_DOWN_BUTTON =>
                  GL_Mouse.State.Wheel := Rho.Mouse.Move_Down;
            end case;
         else
            GL_Mouse.State.Wheel := Rho.Mouse.None;
         end if;
      end if;

      GL_Mouse.State.X := Rho_Float (X);
      GL_Mouse.State.Reverse_Y := Rho_Float (Y);
      GL_Mouse.State.Y := Local_Top_Window.Height - Rho_Float (Y);

   end Mouse_Button_Handler;

   ------------------------
   -- Mouse_Move_Handler --
   ------------------------

   procedure Mouse_Move_Handler
     (X, Y   : Integer)
   is
   begin
      GL_Mouse.State.X := Rho_Float (X);
      GL_Mouse.State.Reverse_Y := Rho_Float (Y);
      GL_Mouse.State.Y := Local_Top_Window.Height - Rho_Float (Y);
   end Mouse_Move_Handler;

   ---------------------
   -- Reshape_Handler --
   ---------------------

   procedure Reshape_Handler
     (Width, Height : Integer)
   is
   begin
      if Log_Operations then
         Rho.Logging.Put_Line ("Reshape_Handler" & Width'Img & Height'Img);
      end if;
      Local_Top_Window.Set_Size
        (Rho_Float (Width), Rho_Float (Height));
      Local_Top_Window.After_Resize;
   end Reshape_Handler;

   -----------------
   -- Save_Matrix --
   -----------------

   overriding procedure Save_Matrix
     (Window : not null access Rho_GL_Window_Record;
      Matrix : in Rho.Matrices.Matrix_Mode_Type)
   is
   begin
      Window.Activate_Shader;

      Window.Current_Shader.Uniform_Matrix_Value (Matrix).Set_Value
        (Window.Current (Matrix));
      Window.Set_Matrix_Saved (Matrix);

   end Save_Matrix;

   ---------------------------
   -- Set_Back_Face_Removal --
   ---------------------------

   overriding procedure Set_Back_Face_Removal
     (Window : in out Rho_GL_Window_Record;
      Enabled : Boolean)
   is
      pragma Unreferenced (Window);
   begin
      if Enabled then
         GL.Enable (GL_CULL_FACE);
      else
         GL.Disable (GL_CULL_FACE);
      end if;
   end Set_Back_Face_Removal;

   ---------------------
   -- Set_Clear_Color --
   ---------------------

   overriding procedure Set_Clear_Color
     (Item  : in out Rho_GL_Window_Record;
      Color : Rho.Color.Rho_Color)
   is
   begin
      Rho.Render_Window.Rho_Render_Window_Record (Item).Set_Clear_Color
        (Color);
      GL.Clear_Color (Clampf (Color.Red),
                      Clampf (Color.Green),
                      Clampf (Color.Blue),
                      Clampf (Color.Alpha));
   end Set_Clear_Color;

   --------------------
   -- Set_Depth_Test --
   --------------------

   overriding procedure Set_Depth_Test
     (Window  : in out Rho_GL_Window_Record;
      Enabled : Boolean)
   is
   begin
      Rho.Render_Window.Rho_Render_Window_Record (Window).Set_Depth_Test
        (Enabled);
      if Enabled then
         GL.Enable (GL_DEPTH_TEST);
         GL.Depth_Function (GL_GREATER);
         GL.Clear_Depth (0.0);
      else
         GL.Disable (GL_DEPTH_TEST);
      end if;
   end Set_Depth_Test;

   ---------------------
   -- Set_Full_Screen --
   ---------------------

   overriding procedure Set_Full_Screen
     (Window      : in out Rho_GL_Window_Record;
      Full_Screen : Boolean)
   is
      pragma Unreferenced (Window);
   begin
      if Full_Screen then
         GLUT.Full_Screen;
      else
         null;
      end if;
--        if Full_Screen then
--           Window.Top_Window.Fullscreen;
--        end if;
   end Set_Full_Screen;

   -------------------------
   -- Set_Output_Position --
   -------------------------

   overriding procedure Set_Output_Position
     (Target             : not null access Rho_GL_Window_Record;
      X, Y               : Rho_Float)
   is
      pragma Unreferenced (Target);
      pragma Unreferenced (X);
      pragma Unreferenced (Y);
   begin
      null;
--        GL.Raster_Pos (GLdouble (X), GLdouble (Y));
   end Set_Output_Position;

   -------------------------
   -- Set_Raster_Position --
   -------------------------

   overriding procedure Set_Raster_Position
     (Target             : not null access Rho_GL_Window_Record;
      Position           : Rho.Matrices.Vector_3)
   is
      pragma Unreferenced (Target);
      pragma Unreferenced (Position);
   begin
      null;
--        GL.Raster_Pos (GLdouble (Position (1)),
--                       GLdouble (Position (2)),
--                       GLdouble (Position (3)));
   end Set_Raster_Position;

   -------------------
   -- Set_Rectangle --
   -------------------

   overriding procedure Set_Rectangle
     (Item  : in out Rho_GL_Window_Record;
      Rectangle : Rho.Rectangle.Rho_Rectangle)
   is
   begin
      Item.Rectangle := Rectangle;
--        Item.Top_Window.Set_Default_Size
--          (Glib.Gint (Rectangle.Width), Glib.Gint (Rectangle.Height));
      Item.Full_Viewport.Set_Size (Rectangle.Width, Rectangle.Height);
   end Set_Rectangle;

   -----------------
   -- Set_Texture --
   -----------------

   overriding procedure Set_Texture
     (Target  : not null access Rho_GL_Window_Record;
      Texture : Rho.Texture.Rho_Texture)
   is
      use type Rho.Texture.Rho_Texture;
   begin
      if Texture /= Target.Texture then
         Target.Texture := Texture;
         if Texture = null then
            if Target.Texture_Enabled then
               GL.Disable (GL_TEXTURE_2D);
               Target.Texture_Enabled := False;
            end if;
         else
            if not Target.Texture_Enabled then
               GL.Enable (GL_TEXTURE_2D);
               Target.Texture_Enabled := True;
            end if;

            if not Texture.Loaded then
               Texture.Load;
            else
               GL.Bind_Texture (GL_TEXTURE_2D, Uint (Texture.Id));
            end if;

            if Texture.Has_Uniform then
               Texture.Uniform.Set_Value (0);
            end if;

         end if;
      end if;
   end Set_Texture;

   ----------------------
   -- Set_Vertex_Array --
   ----------------------

--     overriding procedure Set_Vertex_Array
--       (Target       : not null access Rho_GL_Window_Record;
--        Vertex_Array : Rho.Vertex_Array.Rho_Vertex_Array;
--        Operation    : Rho.Render_Operation.Operation_Type :=
--          Rho.Render_Operation.Triangle_List)
--     is
--        use Rho.Render_Operation;
--        pragma Unreferenced (Target);
--
--        To_GL_Operation : constant array (Operation_Type) of GLenum :=
--                            (Point_List     => GL_POINTS,
--                             Line_List      => GL_LINES,
--                             Line_Strip     => GL_LINE_STRIP,
--                             Triangle_List  => GL_TRIANGLES,
--                             Triangle_Strip => GL_TRIANGLE_STRIP,
--                             Triangle_Fan   => GL_TRIANGLE_FAN,
--                             Quad_List      => GL_QUADS);
--     begin
--        GL.Vertex_Attribute_Pointer
--          (Index        => 0,
--           Size         => 3,
--           Element_Type => GL_FLOAT,
--           Normalized   => GL_FALSE,
--           Stride       => 0,
--           Pointer      => 0);
--
--        GL.Enable_Vertex_Attribute_Array (0);
--
--        GL.Bind_Vertex_Array (Uint (Vertex_Array.Id));
--
--        GL.Draw_Arrays
--          (Mode  => To_GL_Operation (Operation),
--           First => 0,
--           Count => Sizei (Vertex_Array.Count));
--
--     end Set_Vertex_Array;

   ------------------
   -- Set_Viewport --
   ------------------

   overriding procedure Set_Viewport
     (Window   : in out Rho_GL_Window_Record;
      Viewport : in Rho.Viewport.Rho_Viewport)
   is
   begin
      if Log_Operations then
         Rho.Logging.Put ("GL.Viewport ");
         Rho.Logging.Put (Rho.Matrices.Vector_4'
                           (Viewport.X, Viewport.Y,
                            Viewport.Width, Viewport.Height));
         Rho.Logging.New_Line;
      end if;

      Rho.Render_Window.Rho_Render_Window_Record (Window).Set_Viewport
        (Viewport);
      GL.Viewport (Int (Viewport.X), Int (Viewport.Y),
                   Sizei (Viewport.Width), Sizei (Viewport.Height));
   end Set_Viewport;

   -------------------
   -- Set_Wireframe --
   -------------------

   overriding procedure Set_Wireframe
     (Window : in out Rho_GL_Window_Record;
      Enabled : Boolean)
   is
      pragma Unreferenced (Window);
   begin
      if Enabled then
         GL.Polygon_Mode (GL_FRONT_AND_BACK, GL_LINE);
      else
         GL.Polygon_Mode (GL_FRONT_AND_BACK, GL_FILL);
      end if;
   end Set_Wireframe;

   -------------------------
   -- Special_Key_Handler --
   -------------------------

   procedure Special_Key_Handler
     (Key : Integer;
      X, Y : Integer)
   is
      pragma Unreferenced (Key);
   begin
      Mouse_Move_Handler (X, Y);
   end Special_Key_Handler;

   ------------------------
   -- Texture_Coordinate --
   ------------------------

--     overriding procedure Texture_Coordinate
--       (Item    : in out Rho_GL_Render_Operator;
--        S, T    : Rho_Float)
--     is
--        pragma Unreferenced (Item);
--        use Rho.Texture;
--     begin
--  --      if Texture.Loaded then
--  --           if Rho_Texture (Texture) /= Item.Current_Texture then
--  --              Item.Current_Texture := Rho_Texture (Texture);
--  --              GL.BindTexture (GL.TEXTURE_2D, GL.Uint (Texture.Id));
--  --           end if;
--
--        GL.Tex_Coord (GL.Double (S), GL.Double (T));
--    --    end if;
--     end Texture_Coordinate;

   -------------------------
   -- Uniform_Float_Array --
   -------------------------

   overriding procedure Uniform_Float_Array
     (Window   : not null access Rho_GL_Window_Record;
      Uniform  : Rho.Shader.Rho_Uniform_Value;
      Value    : Rho.Float_Arrays.Real_Vector)
   is
   begin
      Window.Activate_Shader;
      Uniform.Set_Value (Value);
   end Uniform_Float_Array;

end Rho.Rendering.GL_Renderer.GL_Window;
