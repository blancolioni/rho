package body Rho.Shaders is

   type Attribute_Binding is
     new Root_Shader_Variable_Type with null record;

   overriding function Is_Attribute
     (Binding : Attribute_Binding)
      return Boolean
   is (True);

   type Uniform_Binding is
     new Root_Shader_Variable_Type with null record;

   overriding function Is_Uniform
     (Binding : Uniform_Binding)
      return Boolean
   is (True);

   ------------
   -- Create --
   ------------

   function Create
     (Name   : String;
      Stage  : Shader_Stage;
      Source : String)
      return Shader_Type
   is
   begin
      return Shader : constant Shader_Type :=
        new Root_Shader_Type'
          (Rho.Objects.Root_Object_Type with
             Stage => Stage,
             Source => Ada.Strings.Unbounded.To_Unbounded_String (Source))
      do
         Shader.Set_Name (Name);
      end return;
   end Create;

   ------------------------------
   -- Create_Attribute_Binding --
   ------------------------------

   function Create_Attribute_Binding
     (Program       : not null access Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive)
      return Shader_Variable_Type
   is
   begin
      return Binding : constant Shader_Variable_Type :=
        new Attribute_Binding'
          (Rho.Objects.Root_Object_Type with
             Program => Program_Type (Program),
             Element_Count => Element_Count)
      do
         Binding.Set_Name (Name);
      end return;

   end Create_Attribute_Binding;

   --------------------
   -- Create_Program --
   --------------------

   function Create_Program (Name : String) return Program_Type is

      procedure Check_Variable
        (Program  : Program_Type;
         Variable : Shader_Variable_Type);

      --------------------
      -- Check_Variable --
      --------------------

      procedure Check_Variable
        (Program  : Program_Type;
         Variable : Shader_Variable_Type)
      is
      begin
         if Variable /= null then
            Program.Shader_Variables.Append (Variable);
         end if;
      end Check_Variable;

   begin
      return Program : constant Program_Type := new Root_Program_Type do
         Program.Set_Name (Name);
         Program.Vertex_Position :=
           Program.Create_Attribute_Binding ("position", 3);
         Program.Vertex_Color :=
           Program.Create_Attribute_Binding ("color", 4);
         Program.Model_View :=
           Program.Create_Uniform_Binding ("model_view");
         Program.Projection :=
           Program.Create_Uniform_Binding ("projection");
         Check_Variable (Program, Program.Vertex_Position);
         Check_Variable (Program, Program.Vertex_Color);
         Check_Variable (Program, Program.Model_View);
         Check_Variable (Program, Program.Projection);
      end return;
   end Create_Program;

   ----------------------------
   -- Create_Uniform_Binding --
   ----------------------------

   function Create_Uniform_Binding
     (Program : not null access Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive := 1)
      return Shader_Variable_Type
   is
   begin
      return Binding : constant Shader_Variable_Type :=
        new Uniform_Binding'
          (Rho.Objects.Root_Object_Type with
             Program => Program_Type (Program), Element_Count => Element_Count)
      do
         Binding.Set_Name (Name);
      end return;
   end Create_Uniform_Binding;

   -----------------------
   -- Iterate_Variables --
   -----------------------

   procedure Iterate_Variables
     (Program : in out Root_Program_Type'Class;
      Process : not null access
        procedure (Variable : not null access
                       Root_Shader_Variable_Type'Class))
   is
   begin
      for Variable of Program.Shader_Variables loop
         Process (Variable);
      end loop;
   end Iterate_Variables;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source (Shader : in out Root_Shader_Type;
                         Source : String)
   is
   begin
      Shader.Source := Ada.Strings.Unbounded.To_Unbounded_String (Source);
   end Set_Source;
end Rho.Shaders;
