with Ada.Text_IO;

with WL.String_Maps;

with Rho.Buffers;
with Rho.Matrices;
with Rho.Shaders;
with Rho.Signals;

with Rho.Shaders.Programs;
with Rho.Shaders.Slices;
with Rho.Shaders.Stages;
with Rho.Shaders.Variables;

with Rho.Values;

with Rho.Matrices.Logs;

with Rho.Version;

with Tau.Generators;

package body Rho.Handles.Simulation is

   package Buffer_Maps is
     new WL.String_Maps (Rho.Buffers.Buffer_Type, Rho.Buffers."=");

   type Simulation_Asset_Container is
     new Rho.Assets.Root_Asset_Container_Type with
      record
         null;
      end record;

   overriding function Generator_Name
     (Container : Simulation_Asset_Container)
      return String
   is ("glsl");

   overriding function Create_Texture_From_Image
     (Container : in out Simulation_Asset_Container;
      Identifier : String)
      return Rho.Assets.Texture_Access;

   type Simulation_Render_Target is
     new Rho.Signals.Signal_Dispatcher
     and Rho.Render.Render_Target with
      record
         Assets            : Rho.Assets.Asset_Container_Type;
         Active_Program    : Rho.Shaders.Programs.Program_Type;
         Active_Int_Buffer : Rho.Buffers.Buffer_Type;
         Bound_Variables   : Buffer_Maps.Map;
         Active_Projection : Rho.Matrices.Matrix_4;
         Active_Model_View : Rho.Matrices.Matrix_4;
         Current_Count     : Natural := 0;
      end record;

   overriding function Assets
     (Target : Simulation_Render_Target)
      return Rho.Assets.Asset_Container_Type
   is (Target.Assets);

   overriding procedure Activate_Shader
     (Target : in out Simulation_Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type);

   No_Active_Shaders : Rho.Render.Active_Shader_Array (1 .. 0);

   overriding function Active_Shaders
     (Target : Simulation_Render_Target)
      return Rho.Render.Active_Shader_Array
   is (No_Active_Shaders);

   overriding procedure Add_Uniform
     (Target  : in out Simulation_Render_Target;
      Name    : String;
      Value   : Rho.Values.Rho_Value)
   is null;

   overriding procedure Bind_Shader
     (Target : in out Simulation_Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type);

   overriding procedure Compile_Shader
     (Target : in out Simulation_Render_Target;
      Shader    : Rho.Shaders.Stages.Shader_Type)
   is null;

   overriding function Create_Program
     (Target : in out Simulation_Render_Target;
      Name      : String;
      Shaders   : Rho.Shaders.Stages.Shader_Array)
      return Rho.Shaders.Programs.Program_Type;

   overriding function Current_Shader
     (Target : Simulation_Render_Target)
      return Rho.Shaders.Programs.Program_Type
   is (Target.Active_Program);

   overriding function Generator
     (Target : Simulation_Render_Target)
      return Tau.Generators.Root_Tau_Generator'Class
   is (Tau.Generators.Null_Generator);

   overriding procedure Set_Projection_Matrix
     (Target : in out Simulation_Render_Target;
      Matrix : Rho.Matrices.Matrix_4);

   overriding procedure Set_Model_View_Matrix
     (Target : in out Simulation_Render_Target;
      Matrix : Rho.Matrices.Matrix_4);

   overriding procedure Set_Camera_World_Matrix
     (Target   : in out Simulation_Render_Target;
      Position : Rho.Matrices.Matrix_4)
   is null;

   overriding procedure Render_Current_Buffers
     (Target : in out Simulation_Render_Target);

   overriding procedure Load_Buffer
     (Target : in out Simulation_Render_Target;
      Buffer  : Rho.Buffers.Buffer_Type);

   overriding procedure Activate_Buffer
     (Target   : in out Simulation_Render_Target;
      Buffer   : Rho.Buffers.Buffer_Type;
      Argument : not null access
        Rho.Shaders.Variables.Root_Variable_Type'Class);

   overriding procedure Add_Shader_Fragment
     (Target   : in out Simulation_Render_Target;
      Slice    : Rho.Shaders.Slices.Slice_Type)
   is null;

   No_Fragments : Rho.Shaders.Slices.Slice_Array (1 .. 0);

   overriding function Active_Shader_Slices
     (Target   : Simulation_Render_Target)
      return Rho.Shaders.Slices.Slice_Array
   is (No_Fragments);

   Local_Render_Target : aliased Simulation_Render_Target :=
     Simulation_Render_Target'
       (Rho.Signals.Signal_Dispatcher with
        Assets            => new Simulation_Asset_Container,
        others            => <>);

   type Simulation_Handle is
     new Root_Handle_Type with
      record
         null;
      end record;

   overriding function Create_Window
     (Handle : in out Simulation_Handle;
      X      : Real;
      Y      : Real;
      Width  : Non_Negative_Real;
      Height : Non_Negative_Real;
      Full   : Boolean)
      return Rho.Windows.Window_Type;

   overriding procedure Main_Loop
     (Handle : in out Simulation_Handle);

   overriding function Current_Renderer
     (Handle : Simulation_Handle)
      return not null access Rho.Render.Render_Target'Class
   is (Local_Render_Target'Access);

   Local_Handle : aliased Simulation_Handle;

   type Simulation_Window is
     new Rho.Windows.Root_Window_Type with
      record
         null;
      end record;

   overriding procedure Before_Render
     (Window : in out Simulation_Window);

   overriding procedure After_Render
     (Window : in out Simulation_Window);

   overriding function Render_Target
     (Window : Simulation_Window)
      return access Rho.Render.Render_Target'Class
   is (Local_Render_Target'Access);

   ---------------------
   -- Activate_Buffer --
   ---------------------

   overriding procedure Activate_Buffer
     (Target   : in out Simulation_Render_Target;
      Buffer   : Rho.Buffers.Buffer_Type;
      Argument : not null access
        Rho.Shaders.Variables.Root_Variable_Type'Class)
   is
   begin
      case Buffer.Contents is
         when Rho.Buffers.Integer_Data =>
            Target.Active_Int_Buffer := Buffer;
         when others =>
            if Target.Bound_Variables.Contains (Argument.Name) then
               Target.Bound_Variables.Replace
                 (Argument.Name, Buffer);
            else
               Target.Bound_Variables.Insert (Argument.Name, Buffer);
            end if;
      end case;
   end Activate_Buffer;

   ---------------------
   -- Activate_Shader --
   ---------------------

   overriding procedure Activate_Shader
     (Target : in out Simulation_Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type)
   is
   begin
      Target.Active_Program := Shader;
   end Activate_Shader;

   ------------------
   -- After_Render --
   ------------------

   overriding procedure After_Render
     (Window : in out Simulation_Window)
   is
      pragma Unreferenced (Window);
   begin
      null;
   end After_Render;

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Window : in out Simulation_Window)
   is null;

   -----------------
   -- Bind_Shader --
   -----------------

   overriding procedure Bind_Shader
     (Target : in out Simulation_Render_Target;
      Shader : Rho.Shaders.Programs.Program_Type)
   is
   begin
      Target.Activate_Shader (Shader);
   end Bind_Shader;

   --------------------
   -- Create_Program --
   --------------------

   overriding function Create_Program
     (Target : in out Simulation_Render_Target;
      Name      : String;
      Shaders   : Rho.Shaders.Stages.Shader_Array)
      return Rho.Shaders.Programs.Program_Type
   is
      pragma Unreferenced (Target, Shaders);
   begin
      return Program : constant Rho.Shaders.Programs.Program_Type :=
        Rho.Shaders.Programs.Create_Program (Name);
   end Create_Program;

   -------------------------------
   -- Create_Texture_From_Image --
   -------------------------------

   overriding function Create_Texture_From_Image
     (Container : in out Simulation_Asset_Container;
      Identifier : String)
      return Rho.Assets.Texture_Access
   is
   begin
      return null;
   end Create_Texture_From_Image;

   -------------------
   -- Create_Window --
   -------------------

   overriding function Create_Window
     (Handle : in out Simulation_Handle;
      X      : Real;
      Y      : Real;
      Width  : Non_Negative_Real;
      Height : Non_Negative_Real;
      Full   : Boolean)
      return Rho.Windows.Window_Type
   is
      pragma Unreferenced (Full);
   begin

      return Window : constant Rho.Windows.Window_Type :=
        new Simulation_Window'
          (Rho.Windows.Root_Window_Type with null record)
      do
         Window.Initialize_Window (X, Y, Width, Height);

         Ada.Text_IO.Put_Line
           ("Rho-Simulation: " & Rho.Version.Version_String);

         Handle.Windows.Append (Window);

      end return;

   end Create_Window;

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle return Handle is
   begin
      return Local_Handle'Access;
   end Get_Handle;

   -----------------
   -- Load_Buffer --
   -----------------

   overriding procedure Load_Buffer
     (Target  : in out Simulation_Render_Target;
      Buffer  : Rho.Buffers.Buffer_Type)
   is
   begin
      null;
   end Load_Buffer;

   ---------------
   -- Main_Loop --
   ---------------

   overriding procedure Main_Loop
     (Handle : in out Simulation_Handle)
   is
   begin
      for Window of Handle.Windows loop
         Window.Render;
      end loop;
   end Main_Loop;

   ----------------------------
   -- Render_Current_Buffers --
   ----------------------------

   overriding procedure Render_Current_Buffers
     (Target : in out Simulation_Render_Target)
   is
      use Rho.Matrices;
      Xs : array (1 .. 3) of Real;
      Count : Natural := 0;

      procedure Add_Coordinate (X : Real);

      --------------------
      -- Add_Coordinate --
      --------------------

      procedure Add_Coordinate (X : Real) is
      begin
         Count := Count + 1;
         Xs (Count) := X;
         if Count = 3 then
            declare
               V1 : constant Rho.Matrices.Vector_4 :=
                 Rho.Matrices.To_Vector (Xs (1), Xs (2), Xs (3), 1.0);
               V2 : constant Rho.Matrices.Vector_4 :=
                 Target.Active_Projection * Target.Active_Model_View
                   * V1;
            begin
               Rho.Matrices.Logs.Log_Transformation
                 ("vertex", V1, V2 / Rho.Matrices.W (V2));
            end;
            Count := 0;
         end if;
      end Add_Coordinate;

   begin

      if Target.Bound_Variables.Contains ("vertex_position") then
         Rho.Matrices.Logs.Log_Matrix
           ("model-view",
            Target.Active_Model_View);
         Rho.Matrices.Logs.Log_Matrix
           ("projection",
            Target.Active_Projection);
         Rho.Matrices.Logs.Log_Matrix
           ("model-view-projection",
            Target.Active_Projection * Target.Active_Model_View);
         Target.Bound_Variables.Element ("vertex_position").Iterate
           (Add_Coordinate'Access);
      end if;
   end Render_Current_Buffers;

   ---------------------------
   -- Set_Model_View_Matrix --
   ---------------------------

   overriding procedure Set_Model_View_Matrix
     (Target : in out Simulation_Render_Target;
      Matrix : Rho.Matrices.Matrix_4)
   is
   begin
      Target.Active_Model_View := Matrix;
   end Set_Model_View_Matrix;

   ---------------------------
   -- Set_Projection_Matrix --
   ---------------------------

   overriding procedure Set_Projection_Matrix
     (Target : in out Simulation_Render_Target;
      Matrix : Rho.Matrices.Matrix_4)
   is
   begin
      Target.Active_Projection := Matrix;
   end Set_Projection_Matrix;

end Rho.Handles.Simulation;
