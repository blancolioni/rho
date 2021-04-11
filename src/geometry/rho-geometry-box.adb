--  with Ada.Text_IO;
--  with Rho.Matrices.Logs;

package body Rho.Geometry.Box is

   type Axis_Type is (X, Y, Z);

   type Axis_Direction is (Up, Down);

   ------------------
   -- Box_Geometry --
   ------------------

   function Box_Geometry
     (Width, Height, Depth : Non_Negative_Real := 1.0)
      return Geometry_Type
   is
      Geometry     : constant Geometry_Type := Create_Geometry;
      Start_Vertex : Vertex_Index := Vertex_Index'First;

      procedure Build_Plane
        (U, V, W        : Axis_Type;
         U_Direction    : Axis_Direction;
         V_Direction    : Axis_Direction;
         Width          : Real;
         Height         : Real;
         Depth          : Real;
         Material       : Material_Index);

      -----------------
      -- Build_Plane --
      -----------------

      procedure Build_Plane
        (U, V, W        : Axis_Type;
         U_Direction    : Axis_Direction;
         V_Direction    : Axis_Direction;
         Width          : Real;
         Height         : Real;
         Depth          : Real;
         Material       : Material_Index)
      is
         DW               : constant Real := Width / 2.0;
         DH               : constant Real := Height / 2.0;
         DD               : constant Real := Depth / 2.0;
         UD               : constant Signed_Unit_Real :=
           (case U_Direction is
               when Up   => 1.0,
               when Down => -1.0);
         VD               : constant Signed_Unit_Real :=
           (case V_Direction is
               when Up   => 1.0,
               when Down => -1.0);
         Vector           : array (Axis_Type) of Real;
         New_Vertex_Start : Vertex_Index := Start_Vertex;
      begin

         Geometry.Begin_Group (Material);

         for Y_Index in 0 .. 1 loop

            declare
               V_Y : constant Real :=
                 Real (Y_Index) * Height - DH;
            begin
               for X_Index in 0 .. 1 loop
                  declare
                     V_X : constant Real := Real (X_Index) * Width - DW;
                  begin
                     Vector (U) := V_X * UD;
                     Vector (V) := V_Y * VD;
                     Vector (W) := DD;

--                       Ada.Text_IO.Put (New_Vertex_Start'Image);
--                       Ada.Text_IO.Put (": ");
--                       Rho.Matrices.Logs.Put_Vector
--                         (Rho.Matrices.To_Vector
--                            (Vector (X), Vector (Y), Vector (Z)));
--                       Ada.Text_IO.New_Line;

                     Geometry.Vertex (Vector (X), Vector (Y), Vector (Z));

                     Vector (U) := 0.0;
                     Vector (V) := 0.0;
                     Vector (W) := (if Depth > 0.0 then 1.0 else -1.0);

                     Geometry.Normal (Vector (X), Vector (Y), Vector (Z));

                     Geometry.Texture (Real (X_Index), Real (1 - Y_Index));

                     New_Vertex_Start := New_Vertex_Start + 1;
                  end;
               end loop;
            end;
         end loop;

         declare
            A : constant Vertex_Index := Start_Vertex;
            B : constant Vertex_Index := Start_Vertex + 2;
            C : constant Vertex_Index := Start_Vertex + 3;
            D : constant Vertex_Index := Start_Vertex + 1;
         begin
--              Ada.Text_IO.Put_Line ("face:" & A'Image & B'Image & D'Image);
--              Ada.Text_IO.Put_Line ("face:" & B'Image & C'Image & D'Image);

            Geometry.Face (A, B, D);
            Geometry.Face (B, C, D);
         end;

         Start_Vertex := New_Vertex_Start;

         Geometry.End_Group;

      end Build_Plane;

   begin
      Build_Plane (Z, Y, X, Down, Down, Depth, Height, Width, 1);
      Build_Plane (Z, Y, X, Up, Down, Depth, Height, -Width, 2);
      Build_Plane (X, Z, Y, Up, Up, Width, Depth, Height, 3);
      Build_Plane (X, Z, Y, Up, Down, Width, Depth, -Height, 4);
      Build_Plane (X, Y, Z, Up, Down, Width, Height, Depth, 5);
      Build_Plane (X, Y, Z, Down, Down, Width, Height, -Depth, 6);
      return Geometry;
   end Box_Geometry;

end Rho.Geometry.Box;
