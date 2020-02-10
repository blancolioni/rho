with WL.String_Maps;

package body Tau.Generators.GLSL is

   package Type_Name_Maps is
     new WL.String_Maps (String);

   Type_Name_Map : Type_Name_Maps.Map;

   type GLSL_Generator_Type is
     new Root_Tau_Generator with
      record
         null;
      end record;

   overriding procedure Start_Shader
     (Generator   : in out GLSL_Generator_Type;
      Name        : String;
      Stage       : Rho.Shader_Stage;
      Environment : Tau.Environment.Tau_Environment);

   overriding procedure End_Shader
     (Generator : in out GLSL_Generator_Type);

   overriding procedure Global_Declaration
     (Generator : in out GLSL_Generator_Type;
      Name      : String;
      Qualifier : Rho.Storage_Qualifier;
      Type_Name : String);

   overriding procedure Local_Declaration
     (Generator      : in out GLSL_Generator_Type;
      Name           : String;
      Type_Name      : String;
      Initialization : String);

   overriding function Shader_Type_Name
     (Generator : GLSL_Generator_Type;
      Tau_Name  : String)
      return String;

   overriding procedure Set_Value
     (Generator : in out GLSL_Generator_Type;
      To_Name   : String;
      Value     : String);

   overriding procedure Return_Value
     (Generator : in out GLSL_Generator_Type;
      Value     : String);

   procedure Main_Statement
     (Generator : in out GLSL_Generator_Type'Class;
      Statement : String);

   procedure Check_Type_Names;

   ----------------------
   -- Check_Type_Names --
   ----------------------

   procedure Check_Type_Names is

      procedure Map (Tau_Name, GLSL_Name : String);

      ---------
      -- Map --
      ---------

      procedure Map (Tau_Name, GLSL_Name : String) is
      begin
         Type_Name_Map.Insert (Tau_Name, GLSL_Name);
      end Map;

   begin
      if Type_Name_Map.Is_Empty then
         Map ("float", "float");
         Map ("integer", "int");

         Map ("vector_2", "vec2");
         Map ("vector_3", "vec3");
         Map ("vector_4", "vec4");

         Map ("matrix_3", "mat3");
         Map ("matrix_4", "mat4");
      end if;
   end Check_Type_Names;

   ----------------
   -- End_Shader --
   ----------------

   overriding procedure End_Shader
     (Generator : in out GLSL_Generator_Type)
   is
   begin
      Generator.Add_Main_Line ("}");
   end End_Shader;

   ------------------------
   -- Global_Declaration --
   ------------------------

   overriding procedure Global_Declaration
     (Generator : in out GLSL_Generator_Type;
      Name      : String;
      Qualifier : Rho.Storage_Qualifier;
      Type_Name : String)
   is
      use all type Rho.Storage_Qualifier;
      Qualifier_Name : constant String :=
                         (case Qualifier is
                             when Input => "in",
                             when Output => "out",
                             when Uniform => "uniform");

   begin
      Generator.Add_Global_Line
        (Qualifier_Name & " " & Type_Name & " " & Name & ";");
   end Global_Declaration;

   --------------------
   -- GLSL_Generator --
   --------------------

   function GLSL_Generator return Root_Tau_Generator'Class is
   begin
      return Generator : constant GLSL_Generator_Type := (others => <>);
   end GLSL_Generator;

   -----------------------
   -- Local_Declaration --
   -----------------------

   overriding procedure Local_Declaration
     (Generator      : in out GLSL_Generator_Type;
      Name           : String;
      Type_Name      : String;
      Initialization : String)
   is
   begin
      Generator.Main_Statement
        (Type_Name & " " & Name &
         (if Initialization = "" then ""
            else " = " & Initialization));
   end Local_Declaration;

   --------------------
   -- Main_Statement --
   --------------------

   procedure Main_Statement
     (Generator : in out GLSL_Generator_Type'Class;
      Statement : String)
   is
   begin
      Generator.Add_Main_Line (Statement & ";");
   end Main_Statement;

   ------------------
   -- Return_Value --
   ------------------

   overriding procedure Return_Value
     (Generator : in out GLSL_Generator_Type;
      Value     : String)
   is
   begin
      case Generator.Shader_Stage is
         when Rho.Fragment_Shader =>
            Generator.Main_Statement ("gl_FragColor = " & Value);
         when Rho.Vertex_Shader =>
            Generator.Main_Statement ("gl_Position = " & Value);
      end case;
   end Return_Value;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (Generator : in out GLSL_Generator_Type;
      To_Name   : String;
      Value     : String)
   is
   begin
      Generator.Main_Statement
        (To_Name & " = " & Value);
   end Set_Value;

   ----------------------
   -- Shader_Type_Name --
   ----------------------

   overriding function Shader_Type_Name
     (Generator : GLSL_Generator_Type;
      Tau_Name  : String)
      return String
   is
      pragma Unreferenced (Generator);
   begin
      Check_Type_Names;
      if Type_Name_Map.Contains (Tau_Name) then
         return Type_Name_Map.Element (Tau_Name);
      else
         raise Constraint_Error with "unknown GLSL type: " & Tau_Name;
      end if;
   end Shader_Type_Name;

   ------------------
   -- Start_Shader --
   ------------------

   overriding procedure Start_Shader
     (Generator   : in out GLSL_Generator_Type;
      Name        : String;
      Stage       : Rho.Shader_Stage;
      Environment : Tau.Environment.Tau_Environment)
   is
   begin
      Root_Tau_Generator (Generator).Start_Shader (Name, Stage, Environment);
      Generator.Add_Global_Line ("#version 330 core");
      Generator.Add_Main_Line ("void main(void) {");
   end Start_Shader;

end Tau.Generators.GLSL;
