with Ada.Directories;
with Ada.Text_IO;

package body Rho.Shaders.Partial.Loader is

   -------------------------
   -- Load_Partial_Shader --
   -------------------------

   function Load_Partial_Shader
     (Stage : Shader_Stage;
      Path  : String)
      return Partial_Shader_Type
   is
      use Ada.Text_IO;
      File : File_Type;
      Shader : Root_Partial_Shader_Type :=
                 Root_Partial_Shader_Type'
                   (Name => Ada.Strings.Unbounded.To_Unbounded_String
                      (Ada.Directories.Base_Name (Path)),
                    Stage => Stage,
                    Lines => <>);
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Shader.Lines.Append (Get_Line (File));
      end loop;
      Close (File);
      return new Root_Partial_Shader_Type'(Shader);
   end Load_Partial_Shader;

end Rho.Shaders.Partial.Loader;
