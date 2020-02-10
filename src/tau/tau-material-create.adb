package body Tau.Material.Create is

   ------------------
   -- New_Material --
   ------------------

   function New_Material
     (Declaration : GCS.Positions.File_Position; Name : String;
      Arguments   : Tau.Declarations.Lists.List;
      Shaders     : Tau.Shaders.Lists.List) return Tau_Material
   is
   begin
      return Material : constant Tau_Material :=
        new Root_Tau_Material'
          (Tau.Objects.Root_Tau_Object with
             Arguments => Arguments,
           Shaders   => Shaders)
      do
         Material.Initialize_Object (Declaration, Name);
      end return;
   end New_Material;

end Tau.Material.Create;
