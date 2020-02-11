with Tau.Objects;
with Tau.Material;

package Tau.Parser is

   function Load_File
     (Path : String)
      return Tau.Objects.Tau_Object;

   function Load_Material
     (Path : String)
      return Tau.Material.Tau_Material
   is (Tau.Material.Tau_Material (Load_File (Path)));

end Tau.Parser;
