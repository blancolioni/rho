with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with Rho.Logging;

package body Rho.Shaders.Builder is

   type Builder_Environment is
     new Rho.Shaders.Partial.Partial_Shader_Environment with
      record
         Names : WL.String_Sets.Set;
      end record;

   overriding function Check
     (This : Builder_Environment;
      Name : String)
      return Boolean
   is (This.Names.Contains (Name));

   function Section_Name (Section : Shader_Section) return String
   is (case Section is
          when Shader_Preamble => "preamble",
          when Shader_Global   => "global",
          when Shader_Local    => "local",
          when Shader_Main     => "main");

   ---------
   -- Add --
   ---------

   procedure Add
     (This    : in out Root_Builder_Type;
      Partial :        Rho.Shaders.Partial.Partial_Shader_Type)
   is
   begin
      This.Names.Include (Partial.Name);
      This.Partials.Append (Partial);
   end Add;

   -----------
   -- Build --
   -----------

   procedure Build (This : in out Root_Builder_Type) is

      procedure Source (Line : String);
      procedure Section (Lines : String_Lists.List);

      procedure Sort_Declarations (Lines : in out String_Lists.List);

      function Starts_With
        (S, Prefix : String)
         return Boolean
      is (S'Length >= Prefix'Length
          and then S (S'First .. S'First + Prefix'Length - 1) = Prefix);

      function Extract_Variable_Name
        (Line : String)
         return String;

      ---------------------------
      -- Extract_Variable_Name --
      ---------------------------

      function Extract_Variable_Name
        (Line : String)
         return String
      is
         use Ada.Characters.Handling;
         Last_Character : Natural := 0;
      begin
         for I in reverse Line'Range loop
            if Is_Alphanumeric (Line (I)) then
               if Last_Character = 0 then
                  Last_Character := I;
               end if;
            else
               if Last_Character > 0 then
                  return Line (I + 1 .. Last_Character);
               end if;
            end if;
         end loop;
         return "";
      end Extract_Variable_Name;

      -------------
      -- Section --
      -------------

      procedure Section (Lines : String_Lists.List) is
      begin
         for Line of Lines loop
            Source (Line);
         end loop;
      end Section;

      -----------------------
      -- Sort_Declarations --
      -----------------------

      procedure Sort_Declarations (Lines : in out String_Lists.List) is

         use Ada.Strings.Unbounded;

         type Declaration_Record is
            record
               Name : Unbounded_String;
               Line : Unbounded_String;
            end record;

         function "<" (Left, Right : Declaration_Record) return Boolean
         is (Index (Right.Line, To_String (Left.Name)) > 0);

         function To_Declaration_Record
           (Line : String)
            return Declaration_Record;

         ---------------------------
         -- To_Declaration_Record --
         ---------------------------

         function To_Declaration_Record
           (Line : String)
            return Declaration_Record
         is
            First_Space : constant Natural :=
                            Ada.Strings.Fixed.Index (Line, " ");
            Second_Space : constant Natural :=
                             (if First_Space = 0 then 0
                              else Ada.Strings.Fixed.Index
                                (Line, " ", First_Space + 1));
         begin
            if First_Space > 0 and then Second_Space > 0 then
               return Declaration_Record'
                 (Name => To_Unbounded_String (Line (First_Space + 1
                  .. Second_Space - 1)),
                  Line => To_Unbounded_String (Line));
            else
               return (others => <>);
            end if;
         end To_Declaration_Record;

         package Declaration_Lists is
           new Ada.Containers.Doubly_Linked_Lists (Declaration_Record);

         package Declaration_Sorting is
           new Declaration_Lists.Generic_Sorting ("<");

         Decls : Declaration_Lists.List;
      begin
         for Line of Lines loop
            Decls.Append (To_Declaration_Record (Line));
         end loop;
         Declaration_Sorting.Sort (Decls);

         Lines.Clear;
         for Decl of Decls loop
            Lines.Append (To_String (Decl.Line));
         end loop;
      end Sort_Declarations;

      ------------
      -- Source --
      ------------

      procedure Source (Line : String) is
         use Ada.Strings.Unbounded;
      begin
         Append (This.Source, Line & Character'Val (10));
         Rho.Logging.Log (Line);
      end Source;

   begin
      for Section in Shader_Section loop
         declare
            Env : Builder_Environment;

            procedure Add_Line (Line : String);

            --------------
            -- Add_Line --
            --------------

            procedure Add_Line (Line : String) is
            begin
               This.Sections (Section).Append (Line);
               if Section = Shader_Global then
                  declare
                     Vrb : constant String := Extract_Variable_Name (Line);
                  begin
                     if Vrb /= "" then
                        if Starts_With (Line, "uniform ") then
                           This.Uniforms.Include (Vrb);
                        elsif Starts_With (Line, "in ") then
                           This.Attributes.Include (Vrb);
                        end if;
                     end if;
                  end;
               end if;
            end Add_Line;

         begin

            Env.Names := This.Names;
            Env.Names.Include (Section_Name (Section));

            for Partial of This.Partials loop
               Partial.Iterate_Lines (Env, Add_Line'Access);
            end loop;
         end;
      end loop;

      Sort_Declarations (This.Sections (Shader_Local));

      Source ("#version 330 core");
      Section (This.Sections (Shader_Preamble));
      Section (This.Sections (Shader_Global));
      Source ("void main() {");
      Section (This.Sections (Shader_Local));
      Section (This.Sections (Shader_Main));
      Source ("}");

      Rho.Logging.Log ("------------------------------------");

   end Build;

end Rho.Shaders.Builder;
