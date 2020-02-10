with Tau.Types;

with Rho.Color;

package Tau.Values is

   type Root_Tau_Value is abstract new Tau.Root_Tau_Node with private;

   type Tau_Value is access all Root_Tau_Value'Class;

   type Tau_Value_Array is array (Positive range <>) of Tau_Value;

   function Value_Type
     (Value : Root_Tau_Value)
      return Tau.Types.Tau_Type;

   function To_Source
     (Value : Root_Tau_Value)
      return String
      is abstract;

   function Real_Value
     (Position : GCS.Positions.File_Position;
      Value    : Rho.Real)
      return Tau_Value;

   function Color
     (Position         : GCS.Positions.File_Position;
      Red, Green, Blue : Rho.Unit_Real;
      Alpha            : Rho.Unit_Real := 1.0)
      return Tau_Value;

   function Color
     (Position  : GCS.Positions.File_Position;
      Rho_Color : Rho.Color.Color_Type)
      return Tau_Value;

private

   type Root_Tau_Value is abstract new Tau.Root_Tau_Node with
      record
         Value_Type : Tau.Types.Tau_Type;
      end record;

   function Value_Type
     (Value : Root_Tau_Value)
      return Tau.Types.Tau_Type
   is (Value.Value_Type);

end Tau.Values;
