with Rho.Material;
with Rho.Nodes;

package Rho.Loaders.Dat is

   function Load
     (Path     : String;
      Material : Rho.Material.Reference_Array)
      return Rho.Nodes.Node_Type;

end Rho.Loaders.Dat;
