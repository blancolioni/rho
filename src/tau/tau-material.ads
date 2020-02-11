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

   type Tau_Material_Array is array (Positive range <>) of Tau_Material;

   function Check
     (Material : Root_Tau_Material)
      return Boolean;

   function Apply
     (Material : Root_Tau_Material'Class;
      Value    : Tau.Values.Tau_Value)
      return Tau_Material;

   function Apply
     (Material : Root_Tau_Material;
      Values   : Tau.Values.Tau_Value_Array)
      return Tau_Material;

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
         Is_Abstract       : Boolean := False;
         Is_Generic        : Boolean := False;
         Generic_Arguments : Tau.Declarations.Lists.List;
         Arguments         : Tau.Declarations.Lists.List;
         Bindings          : Tau.Environment.Tau_Environment;
         Shaders           : Tau.Shaders.Lists.List;
      end record;

   overriding function Children
     (Material : Root_Tau_Material)
      return Tau_Node_Array;

   function Find_Shader
     (Material : Root_Tau_Material'Class;
      Name     : String)
      return Tau.Shaders.Tau_Shader;

   function Apply
     (Material : Root_Tau_Material'Class;
      Value    : Tau.Values.Tau_Value)
      return Tau_Material
   is (Material.Apply ((1 => Value)));

end Tau.Material;
