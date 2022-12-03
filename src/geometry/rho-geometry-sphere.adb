with Rho.Trigonometry;

package body Rho.Geometry.Sphere is

   -----------------------
   -- Sphere_Geometry --
   -----------------------

   function Sphere_Geometry
     (Radius          : Real := 1.0;
      Width_Segments  : Positive := 8;
      Height_Segments : Positive := 8;
      Theta_Start     : Real := 0.0;
      Theta_Length    : Real := 180.0;
      Phi_Start       : Real := 0.0;
      Phi_Length      : Real := 360.0)
      return Geometry_Type
   is
      Geometry     : constant Geometry_Type := Create_Geometry;
      Theta_End    : constant Real :=
                       Real'Min (Theta_Start + Theta_Length, 180.0);
      Index        : Vertex_Index := Vertex_Index'First;
      Grid         : array (0 .. Width_Segments, 0 .. Height_Segments)
        of Vertex_Index;
   begin

      for Y in 0 .. Height_Segments loop
         declare
            use Rho.Trigonometry;
            V : constant Real := Real (Y) / Real (Height_Segments);
            U_Offset : constant Real :=
                         (if Y = 0 and then Theta_Start = 0.0
                          then 0.5 / Real (Width_Segments)
                          elsif Y = Height_Segments
                          and then Theta_End = 180.0
                          then -0.5 / Real (Width_Segments)
                          else 0.0);
            Theta    : constant Angle :=
                         From_Degrees (Theta_Start + V * Theta_Length);
         begin
            for X in 0 .. Width_Segments loop
               declare
                  U : constant Real := Real (X) / Real (Width_Segments);
                  Phi : constant Angle :=
                          From_Degrees (Phi_Start + U * Phi_Length);
                  Vertex : constant Rho.Matrices.Vector_3 :=
                             Rho.Matrices.To_Vector
                               (X => -Radius * Cos (Phi) * Sin (Theta),
                                Y => Radius * Cos (Theta),
                                Z => Radius * Sin (Phi) * Sin (Theta));

               begin

                  Geometry.Vertex (Vertex);
                  Geometry.Normal (Rho.Matrices.Normalize (Vertex));
                  Geometry.Texture (Unit_Clamp (U + U_Offset),
                                    1.0 - V);
                  Grid (X, Y) := Index;
                  Index := Index + 1;
               end;
            end loop;
         end;
      end loop;

      for Y in 0 .. Height_Segments - 1 loop
         for X in 0 .. Width_Segments - 1 loop
            declare
               A : constant Vertex_Index := Grid (X + 1, Y);
               B : constant Vertex_Index := Grid (X, Y);
               C : constant Vertex_Index := Grid (X, Y + 1);
               D : constant Vertex_Index := Grid (X + 1, Y + 1);
            begin
               if Y /= 0 or else Theta_Start > 0.0 then
                  Geometry.Face (A, B, D);
               end if;
               if Y /= Height_Segments - 1 or else Theta_End < 180.0 then
                  Geometry.Face (B, C, D);
               end if;
            end;
         end loop;
      end loop;

      return Geometry;

   end Sphere_Geometry;

end Rho.Geometry.Sphere;
