with Tau.Objects;
with Tau.Types;

with Rho.Color;

package Tau.Values is

   type Root_Tau_Value is abstract new Tau.Root_Tau_Node with private;

   type Tau_Value is access all Root_Tau_Value'Class;

   type Tau_Value_Array is array (Positive range <>) of Tau_Value;

   function Value_Type
     (Value : Root_Tau_Value)
      return Tau.Types.Tau_Type;

   function Has_Source_Text
     (Value : Root_Tau_Value)
      return Boolean;

   function To_Source
     (Value     : Root_Tau_Value;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String
      is abstract;

   function Real_Value
     (Position : GCS.Positions.File_Position;
      Value    : Rho.Real)
      return Tau_Value;

   function Real_Value
     (Value    : Rho.Real)
      return Tau_Value;

   function Color
     (Position         : GCS.Positions.File_Position;
      Red, Green, Blue : Rho.Unit_Real;
      Alpha            : Rho.Unit_Real := 1.0)
      return Tau_Value;

   function Color
     (Red, Green, Blue : Rho.Unit_Real;
      Alpha            : Rho.Unit_Real := 1.0)
      return Tau_Value;

   function Color
     (Position  : GCS.Positions.File_Position;
      Rho_Color : Rho.Color.Color_Type)
      return Tau_Value;

   function Color
     (Rho_Color : Rho.Color.Color_Type)
      return Tau_Value;

   --  function Texture
   --    (Position    : GCS.Positions.File_Position;
   --     Rho_Texture : Rho.Textures.Texture_Type)
   --     return Tau_Value;
   --
   --  function Texture
   --    (Rho_Texture : Rho.Textures.Texture_Type)
   --     return Tau_Value;

   function Object_Value
     (Object : not null access Tau.Objects.Root_Tau_Object'Class)
      return Tau_Value;

private

   type Root_Tau_Value is abstract new Tau.Root_Tau_Node with
      record
         Value_Type : Tau.Types.Tau_Type;
      end record;

   overriding function Class_Name
     (Item : Root_Tau_Value)
      return String
   is ("value");

   function Value_Type
     (Value : Root_Tau_Value)
      return Tau.Types.Tau_Type
   is (Value.Value_Type);

   function Has_Source_Text
     (Value : Root_Tau_Value)
      return Boolean
   is (True);

   function Real_Value
     (Value    : Rho.Real)
      return Tau_Value
   is (Real_Value (GCS.Positions.Null_Position, Value));

   function Color
     (Red, Green, Blue : Rho.Unit_Real;
      Alpha            : Rho.Unit_Real := 1.0)
      return Tau_Value
   is (Color (GCS.Positions.Null_Position, Red, Green, Blue, Alpha));

   function Color
     (Rho_Color : Rho.Color.Color_Type)
      return Tau_Value
   is (Color (GCS.Positions.Null_Position, Rho_Color));

   --  function Texture
   --    (Rho_Texture : Rho.Textures.Texture_Type)
   --     return Tau_Value
   --  is (Texture (GCS.Positions.Null_Position, Rho_Texture));

end Tau.Values;
