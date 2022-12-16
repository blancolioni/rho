with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Rho.Color;
with Rho.Geometry;
with Rho.Matrices;
with Rho.Meshes;

package body Rho.Loaders.Dat is

   Trace : constant Boolean := False;

   type Dat_Command is (Nverts, Nfaces, Vertex, Faces,
                        Textures, Normals, Names);

   function Next_Line (File : Ada.Text_IO.File_Type) return String;

   function Parse_Line
     (Line    : String;
      Command : out Dat_Command;
      Value   : out Integer)
      return Boolean;

   procedure Get_Number
     (Line  : String;
      Index : in out Positive;
      Value : out Real);

   procedure Get_Number
     (Line  : String;
      Index : in out Positive;
      Value : out Integer);

   ----------------
   -- Get_Number --
   ----------------

   procedure Get_Number
     (Line  : String;
      Index : in out Positive;
      Value : out Real)
   is
      Start : Positive;
   begin
      while Index <= Line'Last
        and then Line (Index) in ' ' | ',' | Character'Val (9)
      loop
         Index := Index + 1;
      end loop;

      Start := Index;
      while Index <= Line'Last
        and then Line (Index) in '0' .. '9' | '-' | '+' | '.' | 'e' | 'E'
      loop
         Index := Index + 1;
      end loop;

      Value := Real'Value (Line (Start .. Index - 1));
   end Get_Number;

   ----------------
   -- Get_Number --
   ----------------

   procedure Get_Number
     (Line  : String;
      Index : in out Positive;
      Value : out Integer)
   is
      X : Real;
   begin
      Get_Number (Line, Index, X);
      Value := Integer (X);
   end Get_Number;

   ----------
   -- Load --
   ----------

   function Load
     (Path     : String;
      Material : Rho.Material.Material_Array)
      return Rho.Nodes.Node_Type
   is
      use Ada.Text_IO;
      File : File_Type;
      Num_Faces    : Natural := 0;
      Num_Vertices : Natural := 0;

      package Face_Index_Vectors is
        new Ada.Containers.Vectors (Positive, Positive);

      type Vertex_Record is
         record
            Vertex     : Rho.Matrices.Vector_3;
            Has_Normal : Boolean := False;
            Normal     : Rho.Matrices.Vector_3;
            Faces      : Face_Index_Vectors.Vector;
         end record;

      package Vertices_Container is
        new Ada.Containers.Vectors (Positive, Vertex_Record);

      Vertices : Vertices_Container.Vector;

      type Face_Vertex_Record is
         record
            Index : Positive;
            Tex   : Rho.Matrices.Vector_2;
         end record;

      type Face_Vertex_Array is array (1 .. 3) of Face_Vertex_Record;

      type Face_Record is
         record
            Color    : Rho.Color.Color_Type;
            Normal   : Rho.Matrices.Vector_3;
            Vertices : Face_Vertex_Array;
         end record;

      package Faces_Container is
        new Ada.Containers.Vectors (Positive, Face_Record);

      Face_Vector : Faces_Container.Vector;

      type Group_Record is
         record
            Base, Bound : Positive;
         end record;

      package Group_Vectors is
        new Ada.Containers.Vectors (Positive, Group_Record);

      Group_Vector : Group_Vectors.Vector;

   begin

      Open (File, In_File, Path);

      loop
         declare
            Line : constant String := Next_Line (File);
            Command : Dat_Command;
            Value   : Integer;
         begin
            exit when Line = "" or else Line = "END";

            if not Parse_Line (Line, Command, Value) then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "bad line in dat: " & Line);
               return null;
            end if;

            case Command is
               when Nverts =>
                  Num_Vertices := Value;
               when Nfaces =>
                  Num_Faces := Value;
               when Vertex =>
                  for I in 1 .. Num_Vertices loop
                     declare
                        Vertex_Line : constant String := Next_Line (File);
                        Index       : Positive := Vertex_Line'First;
                        X, Y, Z     : Real;
                     begin
                        Get_Number (Vertex_Line, Index, X);
                        Get_Number (Vertex_Line, Index, Y);
                        Get_Number (Vertex_Line, Index, Z);
                        Vertices.Append
                          (Vertex_Record'
                             (Vertex     => Rho.Matrices.To_Vector (X, Y, Z),
                              Has_Normal => False,
                              Normal     => Rho.Matrices.Zero,
                              Faces      => <>));
                     end;
                  end loop;
               when Faces =>
                  for I in 1 .. Num_Faces loop
                     declare
                        Vertex_Line : constant String := Next_Line (File);
                        Index       : Positive := Vertex_Line'First;
                        R, G, B     : Integer;
                        X, Y, Z     : Real;
                        V           : Natural;
                        Count       : Natural;
                        Face        : Face_Record;
                     begin
                        Get_Number (Vertex_Line, Index, R);
                        Get_Number (Vertex_Line, Index, G);
                        Get_Number (Vertex_Line, Index, B);
                        Face.Color :=
                          (Real (R) / 255.0,
                           Real (G) / 255.0,
                           Real (B) / 255.0,
                           1.0);

                        Get_Number (Vertex_Line, Index, X);
                        Get_Number (Vertex_Line, Index, Y);
                        Get_Number (Vertex_Line, Index, Z);

                        Face.Normal := Rho.Matrices.To_Vector (X, Y, Z);

                        Get_Number (Vertex_Line, Index, Count);
                        pragma Assert (Count = 3);

                        for F of Face.Vertices loop
                           Get_Number (Vertex_Line, Index, V);
                           F.Index := V + 1;
                           Vertices (F.Index).Faces.Append (I);
                        end loop;

                        Face_Vector.Append (Face);

                     end;
                  end loop;

               when Textures =>
                  for Face_Index in 1 .. Face_Vector.Last_Index loop
                     declare
                        Face : Face_Record renames Face_Vector (Face_Index);
                        Texture_Line  : constant String := Next_Line (File);
                        First_Space   : Positive := 1;
                        Index         : Positive;
                        Current_Index : Integer := -1;
                     begin
                        while Texture_Line (First_Space) /= ' '
                          and then Texture_Line (First_Space)
                          /= Character'Val (9)
                        loop
                           First_Space := First_Space + 1;
                        end loop;

                        declare
                           Texture_Name : constant String :=
                                            Ada.Strings.Fixed.Trim
                                              (Texture_Line
                                                 (1 .. First_Space - 1),
                                               Ada.Strings.Both);
                           Mat_Index : constant Natural :=
                                         Natural'Value (Texture_Name);
                        begin
                           if Mat_Index /= Current_Index then
                              if not Group_Vector.Is_Empty then
                                 Group_Vector (Group_Vector.Last_Index)
                                   .Bound := Face_Index;
                              end if;
                              Group_Vector.Append
                                (Group_Record'(Face_Index, Face_Index));
                              Current_Index := Mat_Index;
                           end if;

                           Index := First_Space + 1;
                           declare
                              Max_S, Max_T, S, T : Real;
                           begin
                              Get_Number (Texture_Line, Index, Max_S);
                              Get_Number (Texture_Line, Index, Max_T);

                              for F of Face.Vertices loop
                                 Get_Number (Texture_Line, Index, S);
                                 Get_Number (Texture_Line, Index, T);
                                 F.Tex := Rho.Matrices.To_Vector
                                   (S / Max_S, 1.0 - T / Max_T);
                              end loop;
                           end;

                        end;
                     end;
                  end loop;

                  if not Group_Vector.Is_Empty then
                     Group_Vector (Group_Vector.Last_Index).Bound :=
                       Face_Vector.Last_Index + 1;
                  end if;

               when Names =>
                  Skip_Line (File, Ada.Text_IO.Count (Value));

               when Normals =>
                  for V of Vertices loop
                     declare
                        Vertex_Line : constant String := Next_Line (File);
                        Index       : Positive := Vertex_Line'First;
                        X, Y, Z     : Real;
                     begin
                        Get_Number (Vertex_Line, Index, X);
                        Get_Number (Vertex_Line, Index, Y);
                        Get_Number (Vertex_Line, Index, Z);
                        V.Has_Normal := True;
                        V.Normal     := Rho.Matrices.To_Vector (X, Y, Z);
                     end;
                  end loop;

            end case;
         end;
      end loop;

      Close (File);

      declare
         use type Rho.Geometry.Vertex_Index;
         use type Rho.Matrices.Vector_3;
         Geometry : constant Rho.Geometry.Geometry_Type :=
                      Rho.Geometry.Create_Geometry;
         Index    : Rho.Geometry.Vertex_Index := 1;
         Group_Index : Positive := 1;
      begin
         for Face of Face_Vector loop
            if Face.Normal = Rho.Matrices.Zero then
               declare
                  Normal   : Rho.Matrices.Vector_3 := Rho.Matrices.Zero;
               begin
                  for Vertex of Face.Vertices loop
                     Normal := Normal + Vertices (Vertex.Index).Normal;
                  end loop;
                  Normal := Rho.Matrices.Normalize (Normal);
                  Face.Normal := Normal;
               end;
            end if;
         end loop;

         for Face_Index in 1 .. Face_Vector.Last_Index loop
            declare
               Face : Face_Record renames Face_Vector (Face_Index);
            begin
               for Vertex of Face.Vertices loop
                  Geometry.Vertex (Vertices (Vertex.Index).Vertex);
                  Geometry.Normal (Face.Normal);
                  Geometry.Texture (Rho.Matrices.X (Vertex.Tex),
                                    Rho.Matrices.Y (Vertex.Tex));
               end loop;
            end;
         end loop;

         for Face_Index in 1 .. Face_Vector.Last_Index loop
            if not Group_Vector.Is_Empty
              and then Group_Index <= Group_Vector.Last_Index
              and then Group_Vector (Group_Index).Base = Face_Index
            then
               if Group_Index > 1 then
                  Geometry.End_Group;
               end if;
               Geometry.Begin_Group
                 (Rho.Geometry.Material_Index (Group_Index));
               Group_Index := Group_Index + 1;
            end if;

            Geometry.Face (Index, Index + 1, Index + 2);
            Index := Index + 3;
         end loop;

         if not Group_Vector.Is_Empty then
            Geometry.End_Group;
         end if;

         declare
            Mesh : constant Rho.Meshes.Mesh_Type :=
                     Rho.Meshes.Create_Mesh (Geometry);
         begin
            for Mat of Material loop
               Mesh.Add_Material (Mat);
            end loop;
            Mesh.Set_Name (Ada.Directories.Base_Name (Path));
            return Rho.Nodes.Node_Type (Mesh);
         end;
      end;

   end Load;

   ---------------
   -- Next_Line --
   ---------------

   function Next_Line (File : Ada.Text_IO.File_Type) return String is
   begin
      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Full_Line : constant String := Ada.Text_IO.Get_Line (File);
            Trim_Line : constant String :=
                          Ada.Strings.Fixed.Trim (Full_Line,
                                                  Ada.Strings.Both);
         begin
            if Trim_Line'Length > 1
              and then (Trim_Line (Trim_Line'First) /= '/'
                        or else Trim_Line (Trim_Line'First + 1) /= '/')
            then
               if Trace then
                  Ada.Text_IO.Put_Line (Trim_Line);
               end if;

               return Trim_Line;
            end if;
         end;
      end loop;
      return "";
   end Next_Line;

   ----------------
   -- Parse_Line --
   ----------------

   function Parse_Line
     (Line    : String;
      Command : out Dat_Command;
      Value   : out Integer)
      return Boolean
   is
      Index : Positive := Line'First;
   begin
      while Index <= Line'Last
        and then Line (Index) /= ' '
        and then Line (Index) /= Character'Val (9)
      loop
         Index := Index + 1;
      end loop;
      begin
         Command := Dat_Command'Value (Line (Line'First .. Index - 1));
      exception
         when Constraint_Error =>
            return False;
      end;

      if Index < Line'Last then
         begin
            Value := Integer'Value (Line (Index + 1 .. Line'Last));
         exception
            when Constraint_Error =>
               return False;
         end;
      else
         Value := 0;
      end if;
      return True;
   end Parse_Line;

end Rho.Loaders.Dat;
