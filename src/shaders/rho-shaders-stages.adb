with Rho.Logging;

package body Rho.Shaders.Stages is

   type Source_Writer is
     new Rho.Shaders.Slices.Source_Writer_Interface with
      record
         Source : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Append
     (Writer : in out Source_Writer;
      Line   : String);

   ------------------
   -- Add_Slice --
   ------------------

   procedure Add_Slice
     (Shader   : in out Root_Shader_Type;
      Slice :        Rho.Shaders.Slices.Slice_Type)
   is
   begin
      if Slice.Stage = Shader.Stage then
         Shader.Slices (Slice.Source_Section).Append (Slice);
      end if;
   end Add_Slice;

   -------------------
   -- Add_Slices --
   -------------------

   procedure Add_Slices
     (Shader    : in out Root_Shader_Type;
      Slices :        Rho.Shaders.Slices.Slice_Array)
   is
   begin
      for Slice of Slices loop
         Shader.Add_Slice (Slice);
      end loop;
   end Add_Slices;

   ----------------
   -- Add_Slices --
   ----------------

   procedure Add_Slices
     (Shader    : in out Root_Shader_Type;
      Slices    : Rho.Shaders.Slices.Slice_Container_Interface'Class)
   is
   begin
      Shader.Add_Slices (Slices.Shader_Slices);
   end Add_Slices;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Writer : in out Source_Writer;
      Line   : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Rho.Logging.Log (Line);

      Writer.Source := Writer.Source & Line;
      --  if Line (Line'First) = '#' then
      Writer.Source := Writer.Source & Character'Val (10);
      --  end if;
   end Append;

   -----------
   -- Build --
   -----------

   procedure Build (Shader : in out Root_Shader_Type) is

      use Rho.Shaders.Slices;

      function Before
        (Left, Right : Rho.Shaders.Slices.Slice_Type)
         return Boolean
      is (Left.Source_Priority < Right.Source_Priority);

      package Fragment_Sorting is
        new Fragment_Lists.Generic_Sorting (Before);

      Writer : Source_Writer;

      procedure Generate (Section : Shader_Source_Section);

      --------------
      -- Generate --
      --------------

      procedure Generate (Section : Shader_Source_Section) is
      begin
         for Slice of Shader.Slices (Section) loop
            Slice.Generate (Writer);
         end loop;
      end Generate;

   begin
      for List of Shader.Slices loop
         Fragment_Sorting.Sort (List);
      end loop;

      Generate (Shader_Preamble);
      Generate (Global_Variable);
      Generate (Global_Declaration);
      Writer.Append ("void main() {");
      Generate (Main_Function);
      Writer.Append ("}");

      Shader.Source := Writer.Source;

   end Build;

   ------------
   -- Create --
   ------------

   function Create
     (Name   : String;
      Stage  : Shader_Stage;
      Source : String := "")
      return Shader_Type
   is
   begin
      return Shader : constant Shader_Type :=
        new Root_Shader_Type'
          (Rho.Objects.Root_Object_Type with
             Stage     => Stage,
           Source    =>
             (if Source = "" then Ada.Strings.Unbounded.Null_Unbounded_String
              else Ada.Strings.Unbounded.To_Unbounded_String (Source)),
           Slices    => (others => <>))
      do
         Shader.Set_Name (Name);
      end return;
   end Create;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source
     (Shader : in out Root_Shader_Type;
      Source : String)
   is
   begin
      Shader.Source := Ada.Strings.Unbounded.To_Unbounded_String (Source);
   end Set_Source;

end Rho.Shaders.Stages;
