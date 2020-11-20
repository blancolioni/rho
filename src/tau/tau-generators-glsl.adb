with WL.String_Maps;

package body Tau.Generators.GLSL is

   package Type_Name_Maps is
     new WL.String_Maps (String);

   Type_Name_Map     : Type_Name_Maps.Map;
   Function_Name_Map : Type_Name_Maps.Map;

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

   overriding procedure Freeze
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

   overriding function Shader_Function_Name
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

   begin
      if Type_Name_Map.Is_Empty then
         declare

            procedure Map (Tau_Name, GLSL_Name : String);

            ---------
            -- Map --
            ---------

            procedure Map (Tau_Name, GLSL_Name : String) is
            begin
               Type_Name_Map.Insert (Tau_Name, GLSL_Name);
            end Map;

         begin
            Map ("float", "float");
            Map ("integer", "int");

            Map ("vector_2", "vec2");
            Map ("vector_3", "vec3");
            Map ("vector_4", "vec4");

            Map ("matrix_3", "mat3");
            Map ("matrix_4", "mat4");

            Map ("texture_2d", "sampler2D");
         end;

         declare

            procedure Map (Tau_Name, GLSL_Name : String);

            ---------
            -- Map --
            ---------

            procedure Map (Tau_Name, GLSL_Name : String) is
            begin
               Function_Name_Map.Insert (Tau_Name, GLSL_Name);
            end Map;

         begin
            Map ("sample", "texture");
         end;

      end if;
   end Check_Type_Names;

   ----------------
   -- End_Shader --
   ----------------

   overriding procedure End_Shader
     (Generator : in out GLSL_Generator_Type)
   is
   begin
      null;
   end End_Shader;

   ------------
   -- Freeze --
   ------------

   overriding procedure Freeze
     (Generator : in out GLSL_Generator_Type)
   is
   begin
      for Stage in Rho.Shader_Stage loop
         if Generator.Shaders (Stage).Started then
            Generator.Set_Current_Stage (Stage);
            Generator.Add_Main_Line ("}");
         end if;
      end loop;
   end Freeze;

   ------------------------
   -- Global_Declaration --
   ------------------------

   overriding procedure Global_Declaration
     (Generator : in out GLSL_Generator_Type;
      Name      : String;
      Qualifier : Rho.Storage_Qualifier;
      Type_Name : String)
   is
      use all type Rho.Shader_Stage;
      use all type Rho.Storage_Qualifier;
      Qualifier_Name : constant String :=
                         (case Qualifier is
                             when Input => "in",
                             when Output => "out",
                             when Uniform => "uniform");

      Stage : constant Rho.Shader_Stage := Generator.Current_Stage;

   begin
      if Stage = Rho.Vertex_Shader
        or else Qualifier /= Input
      then
         Generator.Add_Global_Line
           (Qualifier_Name & " " & Type_Name & " " & Name & ";");
      else
         Generator.Set_Current_Stage (Rho.Vertex_Shader);
         Generator.Add_Global_Line
           (Qualifier_Name & " " & Type_Name & " " & Name & ";");
         Generator.Add_Global_Line
           ("out " & Type_Name & " " & Stage_Name (Stage) & "_" & Name & ";");
         Generator.Add_Main_Line
           (Stage_Name (Stage) & "_" & Name & " = " & Name & ";");
         Generator.Set_Current_Stage (Stage);
         Generator.Add_Global_Line
           ("in " & Type_Name & " " & Stage_Name (Stage) & "_" & Name & ";");
      end if;
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
      case Generator.Current_Stage is
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

   --------------------------
   -- Shader_Function_Name --
   --------------------------

   overriding function Shader_Function_Name
     (Generator : GLSL_Generator_Type;
      Tau_Name  : String)
      return String
   is
      pragma Unreferenced (Generator);
   begin
      Check_Type_Names;
      if Function_Name_Map.Contains (Tau_Name) then
         return Function_Name_Map.Element (Tau_Name);
      else
         return Tau_Name;
      end if;
   end Shader_Function_Name;

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
