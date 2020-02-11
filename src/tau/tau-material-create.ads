private with Ada.Containers.Indefinite_Holders;

with Tau.Declarations.Lists;
with Tau.Shaders.Lists;

with Tau.Material.Lists;

package Tau.Material.Create is

   function New_Material
     (Declaration     : GCS.Positions.File_Position;
      Is_Generic      : Boolean;
      Is_Abstract     : Boolean;
      Name            : String;
      Generic_Formals : Tau.Declarations.Lists.List;
      Arguments       : Tau.Declarations.Lists.List;
      Shaders         : Tau.Shaders.Lists.List)
      return Tau_Material;

   function New_Abstract_Material
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Arguments   : Tau.Declarations.Lists.List;
      Shaders     : Tau.Shaders.Lists.List) return Tau_Material;

   type Material_Linker is tagged private;

   procedure Append
     (Linker : in out Material_Linker;
      Material : Tau_Material);

   procedure Link
     (Linker   : in out Material_Linker);

   function Instantiate
     (Linker : Material_Linker)
      return Rho.Material.Material_Type;

private

   package Binding_Holders is
     new Ada.Containers.Indefinite_Holders
       (Tau.Values.Tau_Value_Array, Tau.Values."=");

   type Material_Linker is tagged
      record
         Result   : Tau_Material;
         Bindings : Binding_Holders.Holder;
         Main     : Tau_Material;
         List     : Tau.Material.Lists.List;
      end record;

end Tau.Material.Create;
