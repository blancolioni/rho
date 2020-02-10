with GCS.Positions;

with Tau.Objects;
with Tau.Material;
with Tau.Values;

with Tau.Parser;

with Rho.Paths;

package body Rho.Material.Depth is

   ---------------------------
   -- Create_Depth_Material --
   ---------------------------

   function Create_Depth_Material
     (Near, Far : Real)
      return Material_Type
   is
      Object   : constant Tau.Objects.Tau_Object :=
                   Tau.Parser.Load_File
                     (Rho.Paths.Config_File
                        ("material/rho-material-depth.rho"));
      Material : constant Tau.Material.Tau_Material :=
                   Tau.Material.Tau_Material (Object);

   begin
      if not Material.Check then
         raise Constraint_Error with
           "depth material check failed";
      end if;

      return Material.Instantiate
        ((Tau.Values.Real_Value (GCS.Positions.Null_Position, Near),
         Tau.Values.Real_Value (GCS.Positions.Null_Position, Far)));
   end Create_Depth_Material;

end Rho.Material.Depth;
