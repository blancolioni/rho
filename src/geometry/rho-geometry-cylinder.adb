with Rho.Trigonometry;

package body Rho.Geometry.Cylinder is

   -----------------------
   -- Cylinder_Geometry --
   -----------------------

   function Cylinder_Geometry
     (Top_Radius      : Real := 1.0;
      Bottom_Radius   : Real := 1.0;
      Height          : Real := 1.0;
      Radial_Segments : Positive := 8;
      Height_Segments : Positive := 1)
      return Geometry_Type
   is
      Theta_Start  : constant Non_Negative_Real := 0.0;
      Theta_End    : constant Non_Negative_Real := 360.0;
      Open_Ended   : constant Boolean := False;
      Geometry     : constant Geometry_Type := Create_Geometry;

      Half_Height  : constant Real := Height / 2.0;
      Theta_Length : constant Real := Theta_End - Theta_Start;

      Index        : Vertex_Index := Vertex_Index'First;

      procedure Generate_Cap (Top : Boolean);
      procedure Generate_Torso;

      ------------------
      -- Generate_Cap --
      ------------------

      procedure Generate_Cap (Top : Boolean) is
      begin
         null;
      end Generate_Cap;

      --------------------
      -- Generate_Torso --
      --------------------

      procedure Generate_Torso is
         Slope : constant Real :=
                   (Bottom_Radius - Top_Radius) / Height;
         Indices : array (0 .. Height_Segments, 0 .. Radial_Segments)
           of Vertex_Index;

      begin
         for Y in 0 .. Height_Segments loop
            declare
               V : constant Unit_Real := Real (Y) / Real (Height_Segments);
               Radius : constant Real :=
                          V * (Bottom_Radius - Top_Radius) + Top_Radius;
            begin
               for X in 0 .. Radial_Segments loop
                  declare
                     U : constant Unit_Real :=
                           Real (X) / Real (Radial_Segments);
                     Theta : constant Rho.Trigonometry.Angle :=
                               Rho.Trigonometry.From_Degrees
                                 (U * Theta_Length + Theta_Start);
                     Sin_Theta : constant Signed_Unit_Real :=
                                   Rho.Trigonometry.Sin (Theta);
                     Cos_Theta : constant Signed_Unit_Real :=
                                   Rho.Trigonometry.Cos (Theta);
                  begin
                     Geometry.Vertex
                       (X => Radius * Sin_Theta,
                        Y => -V * Height + Half_Height,
                        Z => Radius * Cos_Theta);
                     Geometry.Normal
                       (Rho.Matrices.Normalize
                          (Rho.Matrices.To_Vector
                               (Sin_Theta, Slope, Cos_Theta)));
                     Geometry.Texture (U, 1.0 - V);
                     Indices (Y, X) := Index;
                     Index := Index + 1;
                  end;
               end loop;
            end;
         end loop;

         for X in 0 .. Radial_Segments - 1 loop
            for Y in 0 .. Height_Segments - 1 loop
               declare
                  A : constant Vertex_Index := Indices (Y, X);
                  B : constant Vertex_Index := Indices (Y + 1, X);
                  C : constant Vertex_Index := Indices (Y + 1, X + 1);
                  D : constant Vertex_Index := Indices (Y, X + 1);
               begin
                  Geometry.Face (A, B, D);
                  Geometry.Face (B, C, D);
               end;
            end loop;
         end loop;

      end Generate_Torso;

   begin

      Generate_Torso;
      if not Open_Ended then
         if Top_Radius > 0.0 then
            Generate_Cap (True);
         end if;
         if Bottom_Radius > 0.0 then
            Generate_Cap (False);
         end if;
      end if;

      return Geometry;

   end Cylinder_Geometry;

end Rho.Geometry.Cylinder;
