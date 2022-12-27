package body Rho.Shaders.Stages is

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
           Partials  => <>)
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
