with Tau.Errors;
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
      Material : constant Tau.Material.Tau_Material :=
        Tau.Parser.Load_Material
          (Rho.Paths.Config_File
             ("material/rho-material-depth.rho"));
   begin
      if not Material.Check then
         Tau.Errors.Write_Errors (Material);
         raise Constraint_Error with
           "depth material check failed";
      end if;

      declare
         Depth_Material : constant Material_Type :=
           Material.Instantiate
             ((Tau.Values.Real_Value (Near),
              Tau.Values.Real_Value (Far)));
      begin
         if Depth_Material = null then
            Tau.Errors.Write_Errors (Material);
            raise Constraint_Error with
              "dept material instantiation failed";
         end if;

         return Depth_Material;
      end;

   end Create_Depth_Material;

end Rho.Material.Depth;
