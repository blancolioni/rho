package body Rho.Shaders.Programs is

   ------------------------------
   -- Create_Attribute_Binding --
   ------------------------------

   function Create_Attribute_Binding
     (Program       : not null access Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive)
      return Variables.Variable_Type
   is
   begin
      return Rho.Shaders.Variables.New_Attribute_Binding
        (Program       => Program,
         Name          => Name,
         Element_Count => Element_Count);
   end Create_Attribute_Binding;

   --------------------
   -- Create_Program --
   --------------------

   function Create_Program (Name : String) return Program_Type is

      procedure Check_Variable
        (Program  : Program_Type;
         Variable : Rho.Shaders.Variables.Variable_Type);

      --------------------
      -- Check_Variable --
      --------------------

      procedure Check_Variable
        (Program  : Program_Type;
         Variable : Rho.Shaders.Variables.Variable_Type)
      is
         use type Rho.Shaders.Variables.Variable_Type;
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
         Program.Vertex_Texture :=
           Program.Create_Attribute_Binding ("vertexTextureCoord", 2);
         --  Program.Vertex_Color :=
         --    Program.Create_Attribute_Binding ("color", 4);
         Program.Model_View :=
           Program.Create_Uniform_Binding ("model");
         Program.Projection :=
           Program.Create_Uniform_Binding ("camera");
         Check_Variable (Program, Program.Vertex_Position);
         Check_Variable (Program, Program.Vertex_Texture);
         --  Check_Variable (Program, Program.Vertex_Color);
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
      return Variables.Variable_Type
   is
   begin
      return Rho.Shaders.Variables.New_Uniform_Binding
        (Program       => Program,
         Name          => Name,
         Element_Count => Element_Count);
   end Create_Uniform_Binding;

   -----------------------
   -- Iterate_Variables --
   -----------------------

   procedure Iterate_Variables
     (Program : in out Root_Program_Type'Class;
      Process : not null access
        procedure (Variable : not null access
                     Variables.Root_Variable_Type'Class))
   is
   begin
      for Variable of Program.Shader_Variables loop
         Process (Variable);
      end loop;
   end Iterate_Variables;

end Rho.Shaders.Programs;
