with Ada.Directories;
with Rho.Paths;

with Tau.Generators.Loader;
with Tau.Objects;
with Tau.Parser;

package body Rho.Assets is

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (Container : Root_Asset_Container_Type;
      Name      : String;
      Extension : String)
      return String
   is
      pragma Unreferenced (Container);

      File_Name : constant String := Name & "." & Extension;

      function Search
        (Base : String)
         return String;

      ------------
      -- Search --
      ------------

      function Search
        (Base : String)
         return String
      is
         use Ada.Directories;
         State : Ada.Directories.Search_Type;
      begin
         Start_Search
           (Search    => State,
            Directory => Base,
            Pattern   => "*." & Extension,
            Filter    => (Ordinary_File => True, others => False));

         while More_Entries (State) loop
            declare
               Item : Directory_Entry_Type;
            begin
               Get_Next_Entry (State, Item);

               if Simple_Name (Item) = File_Name then
                  return Full_Name (Item);
               end if;
            end;
         end loop;

         End_Search (State);

         Start_Search
           (Search    => State,
            Directory => Base,
            Pattern   => "*",
            Filter    => (Directory => True, others => False));

         while More_Entries (State) loop
            declare
               Item : Directory_Entry_Type;
            begin
               Get_Next_Entry (State, Item);

               if Simple_Name (Item) /= "."
                 and then Simple_Name (Item) /= ".."
               then
                  declare
                     Result : constant String :=
                       Search (Full_Name (Item));
                  begin
                     if Result /= "" then
                        return Result;
                     end if;
                  end;
               end if;
            end;
         end loop;

         End_Search (State);
         return "";

      end Search;

   begin
      return Search (Rho.Paths.Config_Path);
   end Find_File;

   ------------
   -- Shader --
   ------------

   function Shader
     (Container : in out Root_Asset_Container_Type;
      Name      : String)
      return Rho.Shaders.Shader_Type
   is
      Asset_Class : Root_Asset_Container_Type'Class renames
                      Root_Asset_Container_Type'Class (Container);
      Path        : constant String :=
                      Asset_Class.Find_File (Name, "rho");
   begin
      if Path = "" then
         raise Constraint_Error with
           "cannot find shader file " & Name & ".rho";
      end if;

      declare
         Object  : constant Tau.Objects.Tau_Object :=
                     Tau.Parser.Load_File (Path);
         Shader  : constant Tau.Shaders.Tau_Shader :=
                     Tau.Shaders.Tau_Shader (Object);
      begin
         return Container.Shader (Shader, null);
      end;
   end Shader;

   ------------
   -- Shader --
   ------------

   function Shader
     (Container  : in out Root_Asset_Container_Type;
      Tau_Shader : Tau.Shaders.Tau_Shader;
      Bindings   : Tau.Environment.Tau_Environment)
      return Rho.Shaders.Shader_Type
   is
      Asset_Class : Root_Asset_Container_Type'Class renames
                      Root_Asset_Container_Type'Class (Container);
      Generator   : Tau.Generators.Root_Tau_Generator'Class :=
                    Tau.Generators.Loader.Load_Generator
                      (Asset_Class.Generator_Name);
   begin
      if not Tau_Shader.Compile (Bindings, Generator) then
         raise Constraint_Error with
           "shader " & Tau_Shader.Name & " failed to compile";
      end if;

      return Rho.Shaders.Create
        (Name   => Tau_Shader.Name,
         Stage  => Tau_Shader.Stage,
         Source => Generator.Get_Source);

   end Shader;

   ------------
   -- Shader --
   ------------

--     overriding function Shader
--       (Container : Root_Asset_Container_Type;
--        Stage     : Shader_Stage;
--        Name      : String)
--        return Rho.Shaders.Shader_Type
--     is
--        pragma Unreferenced (Container);
--     begin
--        return Rho.Shaders.Create (Name, Stage);
--     end Shader;

end Rho.Assets;
