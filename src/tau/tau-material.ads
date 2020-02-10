with Rho.Material;

private with Tau.Declarations.Lists;
private with Tau.Shaders.Lists;

with Tau.Environment;
with Tau.Objects;
with Tau.Values;

package Tau.Material is

   type Root_Tau_Material is
     new Tau.Objects.Root_Tau_Object with private;

   type Tau_Material is access all Root_Tau_Material'Class;

   function Check
     (Material : Root_Tau_Material)
      return Boolean;

   function Instantiate
     (Material  : Root_Tau_Material'Class)
      return Rho.Material.Material_Type;

   function Instantiate
     (Material : Root_Tau_Material'Class;
      Argument : Tau.Values.Tau_Value)
      return Rho.Material.Material_Type;

   function Instantiate
     (Material  : Root_Tau_Material;
      Arguments : Tau.Values.Tau_Value_Array)
      return Rho.Material.Material_Type;

private

   type Root_Tau_Material is
     new Tau.Objects.Root_Tau_Object with
      record
         Arguments : Tau.Declarations.Lists.List;
         Shaders   : Tau.Shaders.Lists.List;
      end record;

end Tau.Material;
