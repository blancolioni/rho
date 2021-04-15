with Tau.Shaders.Builder;

package body Tau.Shaders.Library is

   -------------------------
   -- Single_Color_Shader --
   -------------------------

   -------------------------
   -- Single_Color_Shader --
   -------------------------

   function Single_Color_Shader
     (Color : Rho.Color.Color_Type)
      return Tau_Shader
   is
      pragma Unreferenced (Color);
      use Tau.Shaders.Builder;
      Position : constant GCS.Positions.File_Position :=
                   GCS.Positions.Null_Position;
   begin
      return Shader : constant Tau_Shader :=
        New_Shader
          (Position    => Position,
           Name        => "single_color",
           Is_Abstract => False)
      do
         declare
            --  use Tau.Expressions;
            Fragment : constant Tau_Shader_Stage :=
                         New_Shader_Stage (Position, Rho.Fragment_Shader);
         begin
            Add_Uniform_Variable
              (Fragment,
               Tau.Declarations.Global_Variable_Declaration
                 (Position      => Position,
                  Name          => "surface_color",
                  Qualifier     => Rho.Uniform,
                  Type_Name     => "vector_4"));
                  --  Initial_Value =>
                  --    Tau.Expressions.Aggregate
                  --      (Position       => Position,
                  --                 Return_Type    =>
                  --                   Tau.Types.Vectors.Vector (4),
                  --                 Values         =>
                  --                   (Literal (Position, Float (Color.R)),
                  --                    Literal (Position, Float (Color.G)),
                  --                    Literal (Position, Float (Color.B)),
                  --                 Literal (Position, Float (Color.A))))));
            Add_Stage (Shader, Fragment);

         end;
      end return;
   end Single_Color_Shader;

end Tau.Shaders.Library;
