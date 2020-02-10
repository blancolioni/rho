with GCS.Positions;

with Tau.Objects;
with Tau.Material;
with Tau.Values;

with Tau.Parser;

with Rho.Paths;

package body Rho.Material.Basic is

   ---------------------------
   -- Create_Basic_Material --
   ---------------------------

   function Create_Basic_Material
     (Color : Rho.Color.Color_Type)
      return Material_Type
   is
      Object : constant Tau.Objects.Tau_Object :=
                 Tau.Parser.Load_File
                   (Rho.Paths.Config_File
                      ("material/rho-material-basic.rho"));
      Material : constant Tau.Material.Tau_Material :=
                   Tau.Material.Tau_Material (Object);

   begin
      if not Material.Check then
         raise Constraint_Error with
           "basic material check failed";
      end if;

      return Material.Instantiate
        ((1 => Tau.Values.Color (GCS.Positions.Null_Position, Color)));
   end Create_Basic_Material;

end Rho.Material.Basic;
