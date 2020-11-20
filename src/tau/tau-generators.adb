with Rho.Logging;

with Tau.Entries;

package body Tau.Generators is

   ---------------------
   -- Add_Global_Line --
   ---------------------

   procedure Add_Global_Line
     (Generator : in out Root_Tau_Generator; Line : String)
   is
   begin
      Generator.Shaders (Generator.Current_Stage).Global_Lines.Append (Line);
   end Add_Global_Line;

   -------------------
   -- Add_Main_Line --
   -------------------

   procedure Add_Main_Line
     (Generator : in out Root_Tau_Generator; Line : String)
   is
   begin
      Generator.Shaders (Generator.Current_Stage).Main_Lines.Append (Line);
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

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Generator : in out Root_Tau_Generator) is
   begin
      null;
   end Freeze;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source
     (Generator : Root_Tau_Generator;
      Stage     : Rho.Shader_Stage)
      return String
   is
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
           Ada.Strings.Unbounded.To_String
           (Generator.Shaders (Stage).Shader_Name));

      for Line of Generator.Shaders (Stage).Global_Lines loop
         Log_Line (Line);
         Result := Result & Line & NL;
      end loop;
      for Line of Generator.Shaders (Stage).Main_Lines loop
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
      G : Root_Tau_Generator'Class renames
            Root_Tau_Generator'Class (Generator);

      procedure Reference_Entry
        (Item : Tau.Entries.Tau_Entry);

      ---------------------
      -- Reference_Entry --
      ---------------------

      procedure Reference_Entry
        (Item : Tau.Entries.Tau_Entry)
      is
      begin
         if Item.Entry_Type.Has_Uniform_Binding then
            G.Global_Declaration
              (Item.Name, Rho.Uniform,
               G.Shader_Type_Name (Item.Entry_Type.Name));
         end if;
      end Reference_Entry;

   begin

      Generator.Env_Stack.Append (Generator.Environment);
      Generator.Environment := Environment;

      Environment.Iterate (Reference_Entry'Access);

   end Push_Environment;

   -----------------------
   -- Set_Current_Stage --
   -----------------------

   procedure Set_Current_Stage
     (Generator : in out Root_Tau_Generator'Class;
      Stage    : Rho.Shader_Stage)
   is
   begin
      pragma Assert (Generator.Shaders (Stage).Started);
      Generator.Current_Stage := Stage;
   end Set_Current_Stage;

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
      Generator.Shaders (Stage).Shader_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Generator.Shaders (Stage).Started := True;
      Generator.Current_Stage := Stage;
   end Start_Shader;

end Tau.Generators;
