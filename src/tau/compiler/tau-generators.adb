with Ada.Characters.Handling;

with Rho.Shaders.Slices.Attributes;
with Rho.Shaders.Slices.Main;
with Rho.Shaders.Slices.Preamble;
with Rho.Shaders.Slices.Uniforms;
with Rho.Shaders.Stages;

package body Tau.Generators is

   type Null_Generator_Record is
     new Root_Tau_Generator with null record;

   procedure Append_Main
     (Generator : in out Root_Tau_Generator'Class;
      Line      : String);

   -----------------
   -- Append_Main --
   -----------------

   procedure Append_Main
     (Generator : in out Root_Tau_Generator'Class;
      Line      : String)
   is
      use type Rho.Shaders.Slices.Shader_Source_Priority;
   begin
      Generator.Slices.Add_Slice
        (Rho.Shaders.Slices.Main.Shader_Line
           (Stage    => Generator.Current_Stage,
            Priority => Generator.Priority,
            Name     => "",
            Line     => Line));
      Generator.Priority := Generator.Priority + 1;
   end Append_Main;

   ------------------------
   -- Global_Declaration --
   ------------------------

   procedure Global_Declaration
     (Generator : in out Root_Tau_Generator;
      Name      : String;
      Qualifier : Rho.Storage_Qualifier;
      Type_Name : String)
   is
   begin
      case Qualifier is
         when Rho.Input =>
            Generator.Slices.Add_Slice
              (Rho.Shaders.Slices.Attributes.In_Attribute_Fragment
                 (Generator.Current_Stage, Name, Type_Name));
         when Rho.Output =>
            Generator.Slices.Add_Slice
              (Rho.Shaders.Slices.Attributes.Out_Attribute_Fragment
                 (Generator.Current_Stage, Name, Type_Name));
         when Rho.Uniform =>
            Generator.Slices.Add_Slice
              (Rho.Shaders.Slices.Uniforms.Uniform_Fragment
                 (Generator.Current_Stage, Name, Type_Name));
      end case;
   end Global_Declaration;

   -----------------------
   -- Local_Declaration --
   -----------------------

   procedure Local_Declaration
     (Generator      : in out Root_Tau_Generator;
      Name           : String;
      Type_Name      : String;
      Initialization : String)
   is
   begin
      Generator.Append_Main
        (Type_Name & " " & Name & " = " & Initialization);
   end Local_Declaration;

   -------------------------
   -- Normalize_Reference --
   -------------------------

   function Normalize_Reference
     (Generator : Root_Tau_Generator;
      Name      : String)
      return String
   is
      use Ada.Characters.Handling;
      Result : String := Name;
      First  : Boolean := True;
      Upcase : Boolean := False;
      Index  : Positive := Result'First;
   begin
      for Ch of Name loop
         if Ch = '_' then
            Upcase := True;
         else
            if First then
               Result (Index) := To_Lower (Ch);
               First := False;
            elsif Upcase then
               Result (Index) := To_Upper (Ch);
               Upcase := False;
            else
               Result (Index) := Ch;
            end if;
            Index := Index + 1;
         end if;
      end loop;
      return Result (Result'First .. Index - 1);
   end Normalize_Reference;

   --------------------
   -- Null_Generator --
   --------------------

   function Null_Generator return Root_Tau_Generator'Class is
   begin
      return Generator : constant Null_Generator_Record :=
        Null_Generator_Record'
          (others => <>);
   end Null_Generator;

   ------------------
   -- Return_Value --
   ------------------

   procedure Return_Value
     (Generator : in out Root_Tau_Generator'Class;
      Value     : String)
   is
   begin
      case Generator.Current_Stage is
         when Rho.Vertex_Shader =>
            Generator.Append_Main
              ("gl_Position = " & Value);
         when Rho.Fragment_Shader =>
            raise Constraint_Error with
              "fragment shader cannot return values";
      end case;
   end Return_Value;

   ----------------------
   -- Set_Shader_Stage --
   ----------------------

   procedure Set_Shader_Stage
     (Generator : in out Root_Tau_Generator;
      Stage     : Rho.Shader_Stage)
   is
   begin
      Generator.Current_Stage := Stage;
   end Set_Shader_Stage;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Generator   : in out Root_Tau_Generator'Class;
      Destination : String;
      Source      : String)
   is
   begin
      Generator.Append_Main
        (Destination & " = " & Source);
   end Set_Value;

   -------------------
   -- Shader_Source --
   -------------------

   function Shader_Source
     (Generator : Root_Tau_Generator;
      Stage     : Rho.Shader_Stage)
      return String
   is
      Shader : constant Rho.Shaders.Stages.Shader_Type :=
                 Rho.Shaders.Stages.Create
                   (-Generator.Name, Stage);
   begin
      Shader.Add_Slices (Generator.Slices);
      Shader.Build;
      return Shader.Shader_Source;
   end Shader_Source;

   -----------
   -- Start --
   -----------

   procedure Start
     (Generator : in out Root_Tau_Generator'Class;
      Name      : String)
   is
      procedure Function_Name
        (Tau_Name : String;
         Gen_Name : String);

      procedure Type_Name
        (Tau_Name : String;
         Gen_Name : String);

      -------------------
      -- Function_Name --
      -------------------

      procedure Function_Name
        (Tau_Name : String;
         Gen_Name : String)
      is
      begin
         Generator.Function_Names.Insert (Tau_Name, Gen_Name);
      end Function_Name;

      ---------------
      -- Type_Name --
      ---------------

      procedure Type_Name
        (Tau_Name : String;
         Gen_Name : String)
      is
      begin
         Generator.Type_Names.Insert (Tau_Name, Gen_Name);
         Generator.Function_Names.Insert (Tau_Name, Gen_Name);
      end Type_Name;

   begin
      Generator.Type_Names.Clear;
      Generator.Function_Names.Clear;
      Generator.Current_Stage := Rho.Vertex_Shader;
      Generator.Slices := Rho.Shaders.Slices.Empty;
      Generator.Priority :=
        Rho.Shaders.Slices.Shader_Source_Priority'First;

      Generator.Name := +Name;
      Type_Name ("vector_2", "vec2");
      Type_Name ("vector_3", "vec3");
      Type_Name ("vector_4", "vec4");
      Type_Name ("color", "vec4");
      Type_Name ("matrix_2", "mat2");
      Type_Name ("matrix_3", "mat3");
      Type_Name ("matrix_4", "mat4");
      Type_Name ("sampler_2d", "sampler2D");

      Function_Name ("abs", "length");

      for Stage in Rho.Shader_Stage loop
         Generator.Slices.Add_Slice
           (Rho.Shaders.Slices.Preamble.Shader_Preamble
              (Stage));
      end loop;
   end Start;

end Tau.Generators;
