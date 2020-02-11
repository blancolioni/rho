with Rho.Logging;

package body Tau.Generators is

   ---------------------
   -- Add_Global_Line --
   ---------------------

   procedure Add_Global_Line
     (Generator : in out Root_Tau_Generator; Line : String)
   is
   begin
      Generator.Global_Lines.Append (Line);
   end Add_Global_Line;

   -------------------
   -- Add_Main_Line --
   -------------------

   procedure Add_Main_Line
     (Generator : in out Root_Tau_Generator; Line : String)
   is
   begin
      Generator.Main_Lines.Append (Line);
   end Add_Main_Line;

   ----------------
   -- End_Shader --
   ----------------

   procedure End_Shader (Generator : in out Root_Tau_Generator) is
   begin
      null;
   end End_Shader;

   -----------------
   -- Float_Image --
   -----------------

   function Float_Image
     (Generator : Root_Tau_Generator; Value : Float) return String
   is
      pragma Unreferenced (Generator);
   begin
      return Value'Image;
   end Float_Image;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source (Generator : Root_Tau_Generator) return String is
      use Ada.Strings.Unbounded;
      NL         : constant String := (1 => Character'Val (10));
      Result     : Unbounded_String := Null_Unbounded_String;
      Line_Count : Natural := 0;

      procedure Log_Line (Line : String);

      --------------
      -- Log_Line --
      --------------

      procedure Log_Line (Line : String) is
      begin
         Line_Count := Line_Count + 1;

         declare
            Img : constant String := Line_Count'Image;
            Spc : constant String (1 .. 4 - Img'Length) :=
              (others => ' ');
         begin
            Rho.Logging.Log (Spc & Img & ": " & Line);
         end;
      end Log_Line;

   begin
      Rho.Logging.Log
        ("Generating shader: " &
           Ada.Strings.Unbounded.To_String (Generator.Shader_Name));

      for Line of Generator.Global_Lines loop
         Log_Line (Line);
         Result := Result & Line & NL;
      end loop;
      for Line of Generator.Main_Lines loop
         Log_Line (Line);
         Result := Result & Line & NL;
      end loop;
      return To_String (Result);
   end Get_Source;

   -------------------
   -- Integer_Image --
   -------------------

   function Integer_Image
     (Generator : Root_Tau_Generator; Value : Integer) return String
   is
      pragma Unreferenced (Generator);
   begin
      return Value'Image;
   end Integer_Image;

   ---------------------
   -- Pop_Environment --
   ---------------------

   procedure Pop_Environment
     (Generator   : in out Root_Tau_Generator)
   is
   begin
      Generator.Environment := Generator.Env_Stack.Last_Element;
      Generator.Env_Stack.Delete_Last;
   end Pop_Environment;

   ----------------------
   -- Push_Environment --
   ----------------------

   procedure Push_Environment
     (Generator   : in out Root_Tau_Generator;
      Environment : Tau.Environment.Tau_Environment)
   is
   begin
      Generator.Env_Stack.Append (Generator.Environment);
      Generator.Environment := Environment;
   end Push_Environment;

   ------------------
   -- Start_Shader --
   ------------------

   procedure Start_Shader
     (Generator   : in out Root_Tau_Generator;
      Name        : String;
      Stage       : Rho.Shader_Stage;
      Environment : Tau.Environment.Tau_Environment)
   is
   begin
      Generator.Environment := Environment;
      Generator.Shader_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Generator.Shader_Stage := Stage;
   end Start_Shader;

end Tau.Generators;
