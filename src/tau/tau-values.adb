with Rho.Matrices;

with Tau.Types.Scalar;
with Tau.Types.Textures;
with Tau.Types.Vectors;

with Tau.Generators;

package body Tau.Values is

   type Real_Value_Record is
     new Root_Tau_Value with
      record
         Value : Rho.Real;
      end record;

   overriding function To_Source
     (Value     : Real_Value_Record;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String
   is (Tau.Generators.Root_Tau_Generator'Class (Generator)
       .Float_Image (Float (Value.Value)));

   type Vector_4_Value_Record is
     new Root_Tau_Value with
      record
         Value : Rho.Matrices.Vector_4;
      end record;

   pragma Warnings (Off);  --  Generator unreferenced

   overriding function To_Source
     (Value     : Vector_4_Value_Record;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String
   is ("vec4" & Rho.Matrices.Image (Value.Value));

   pragma Warnings (On);

   type Texture_Value_Record is
     new Root_Tau_Value with
      record
         Texture : Rho.Textures.Texture_Type;
      end record;

   overriding function Has_Source_Text
     (Value : Texture_Value_Record)
      return Boolean
   is (False);

   pragma Warnings (Off);

   overriding function To_Source
     (Value     : Texture_Value_Record;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String
   is (raise Constraint_Error with
         "no source representation for " & Value.Texture.Name);

   pragma Warnings (On);

   type Object_Value_Record is
     new Root_Tau_Value with
      record
         Object : Tau.Objects.Tau_Object;
      end record;

   overriding function To_Source
     (Value     : Object_Value_Record;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String;

   -----------
   -- Color --
   -----------

   function Color
     (Position         : GCS.Positions.File_Position;
      Red, Green, Blue : Rho.Unit_Real;
      Alpha            : Rho.Unit_Real := 1.0)
      return Tau_Value
   is
      Result : Vector_4_Value_Record :=
                 Vector_4_Value_Record'
                   (Root_Tau_Node with
                    Value_Type => Tau.Types.Vectors.Vector (4),
                    Value      =>
                      Rho.Matrices.To_Vector (Red, Green, Blue, Alpha));
   begin
      Result.Initialize_Node (Position);
      return new Vector_4_Value_Record'(Result);
   end Color;

   -----------
   -- Color --
   -----------

   function Color
     (Position  : GCS.Positions.File_Position;
      Rho_Color : Rho.Color.Color_Type)
      return Tau_Value
   is
   begin
      return Color (Position, Rho_Color.R, Rho_Color.G,
                    Rho_Color.B, Rho_Color.A);
   end Color;

   ------------------
   -- Object_Value --
   ------------------

   function Object_Value
     (Object : not null access Tau.Objects.Root_Tau_Object'Class)
      return Tau_Value
   is
      Result : Object_Value_Record :=
        Object_Value_Record'
          (Root_Tau_Node with
           Value_Type => Tau.Types.Vectors.Vector (4),
           Object     => Tau.Objects.Tau_Object (Object));
   begin
      Result.Initialize_Node (Object.Defined_At);
      return new Object_Value_Record'(Result);
   end Object_Value;

   ----------------
   -- Real_Value --
   ----------------

   function Real_Value
     (Position : GCS.Positions.File_Position;
      Value    : Rho.Real)
      return Tau_Value
   is
      Result : Real_Value_Record := Real_Value_Record'
        (Root_Tau_Node with
         Value_Type => Tau.Types.Scalar.Tau_Float,
         Value      => Value);
   begin
      Result.Initialize_Node (Position);
      return new Real_Value_Record'(Result);
   end Real_Value;

   -------------
   -- Texture --
   -------------

   function Texture
     (Position    : GCS.Positions.File_Position;
      Rho_Texture : Rho.Textures.Texture_Type)
      return Tau_Value
   is
      Result : Texture_Value_Record := Texture_Value_Record'
        (Root_Tau_Node with
         Value_Type =>
           Tau.Types.Textures.Texture (Rho_Texture.Dimension_Count),
         Texture    => Rho_Texture);
   begin
      Result.Initialize_Node (Position);
      return new Texture_Value_Record'(Result);
   end Texture;

   ---------------
   -- To_Source --
   ---------------

   overriding function To_Source
     (Value     : Object_Value_Record;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String
   is
   begin
      return Value.Object.To_Source (Generator);
   end To_Source;

end Tau.Values;
