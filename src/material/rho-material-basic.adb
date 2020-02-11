with GCS.Positions;

with Tau.Material.Create;
with Tau.Values;

with Tau.Errors;
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
      Basic_Material : constant Tau.Material.Tau_Material :=
          Tau.Parser.Load_Material
            (Rho.Paths.Config_File
               ("material/rho-material-basic.rho"));
      Color_Material : constant Tau.Material.Tau_Material :=
        Tau.Parser.Load_Material
          (Rho.Paths.Config_File
             ("material/rho-material-simple_color.rho"));

      Linker         : Tau.Material.Create.Material_Linker;

   begin
      if not Basic_Material.Check then
         Tau.Errors.Write_Errors (Basic_Material);
         raise Constraint_Error with
           "basic material check failed";
      end if;

      Linker.Append (Basic_Material);

      if not Color_Material.Check then
         raise Constraint_Error with
           "color material check failed";
      end if;

      Linker.Append
        (Color_Material.Apply
           (Tau.Values.Color (Color)));

      Linker.Link;

      return Linker.Instantiate;

   end Create_Basic_Material;

   ---------------------------
   -- Create_Basic_Material --
   ---------------------------

   function Create_Basic_Material
     (Texture : Rho.Textures.Texture_Type)
      return Material_Type
   is
      Basic_Material : constant Tau.Material.Tau_Material :=
        Tau.Parser.Load_Material
          (Rho.Paths.Config_File
             ("material/rho-material-basic.rho"));
      Texture_Material : constant Tau.Material.Tau_Material :=
        Tau.Parser.Load_Material
          (Rho.Paths.Config_File
             ("material/rho-material-textured.rho"));
      Linker         : Tau.Material.Create.Material_Linker;

   begin
      if not Basic_Material.Check then
         raise Constraint_Error with
           "basic material check failed";
      end if;

      Linker.Append (Basic_Material);

      if not Texture_Material.Check then
         raise Constraint_Error with
           "texture material check failed";
      end if;

      Linker.Append
        (Texture_Material.Apply
           (Tau.Values.Texture
                (GCS.Positions.Null_Position, Texture)));

      return Linker.Instantiate;

   end Create_Basic_Material;

end Rho.Material.Basic;
