with Tau.Declarations.Lists;
with Tau.Shaders.Lists;

package Tau.Material.Create is

   function New_Material
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Arguments   : Tau.Declarations.Lists.List;
      Shaders     : Tau.Shaders.Lists.List)
      return Tau_Material;

end Tau.Material.Create;
