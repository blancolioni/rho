with Ada.Directories;
with Rho.Paths;

with Rho.Shaders.Partial.Loader;

package body Rho.Assets is

   ---------------
   -- Find_File --
   ---------------

   overriding function Find_File
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

   --------------------
   -- Partial_Shader --
   --------------------

   function Partial_Shader
     (Container : in out Root_Asset_Container_Type;
      Name      : String;
      Stage     : Shader_Stage)
      return Rho.Shaders.Partial.Partial_Shader_Type
   is
      Extension : constant String :=
                    (case Stage is
                        when Vertex_Shader => "vertex",
                        when Fragment_Shader => "fragment");
      Path      : constant String := Container.Find_File (Name, Extension);
   begin
      if Path = "" then
         return null;
      end if;

      return Rho.Shaders.Partial.Loader.Load_Partial_Shader (Stage, Path);
   end Partial_Shader;

   -------------
   -- Texture --
   -------------

   --  function Texture
   --    (Container : in out Root_Asset_Container_Type;
   --     Name      : String)
   --     return Rho.Textures.Texture_Type
   --  is
   --     Asset_Class : Root_Asset_Container_Type'Class renames
   --       Root_Asset_Container_Type'Class (Container);
   --     Path        : constant String :=
   --       Asset_Class.Find_File (Name, "rho");
   --  begin
   --     if Path = "" then
   --        raise Constraint_Error with
   --          "cannot find texture file " & Name & ".rho";
   --     end if;
   --
   --     declare
   --        Texture : constant Tau.Textures.Tau_Texture :=
   --          Tau.Parser.Load_Texture (Path);
   --     begin
   --        return Texture.Instantiate;
   --     end;
   --
   --  end Texture;

end Rho.Assets;
