with Ada.Strings.Unbounded;

package body Rho.Shaders.Slices.Attributes is

   subtype Attribute_Mode is Variable_Mode range In_Variable .. Out_Variable;

   type Attribute_Slice_Type is
     new Root_Slice_Type with
      record
         Type_Name : Ada.Strings.Unbounded.Unbounded_String;
         Mode      : Attribute_Mode;
      end record;

   overriding procedure Generate
     (Slice : Attribute_Slice_Type;
      Source   : in out Source_Writer_Interface'Class);

   type Attribute_Access is access all Attribute_Slice_Type'Class;

   function Attribute_Fragment
     (Stage : Shader_Stage;
      Name      : String;
      Type_Name : String;
      Mode      : Attribute_Mode)
      return Slice_Type;

   ------------------------
   -- Attribute_Fragment --
   ------------------------

   function Attribute_Fragment
     (Stage : Shader_Stage;
      Name      : String;
      Type_Name : String;
      Mode      : Attribute_Mode)
      return Slice_Type
   is
      Attribute : constant Attribute_Access :=
                    new Attribute_Slice_Type;
   begin
      Attribute.Initialize
        (Name     => Name,
         Stage    => Stage,
         Section  => Global_Variable,
         Priority => 1);
      Attribute.Type_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Type_Name);
      Attribute.Mode := Mode;
      return Slice_Type (Attribute);
   end Attribute_Fragment;

   overriding procedure Generate
     (Slice : Attribute_Slice_Type;
      Source   : in out Source_Writer_Interface'Class)
   is
      Mode : constant String :=
               (case Slice.Mode is
                   when In_Variable => "in",
                   when Out_Variable => "out");
   begin
      Source.Append
        (Mode
         & " "
         & Ada.Strings.Unbounded.To_String (Slice.Type_Name)
         & " "
         & Slice.Name
         & ";");
   end Generate;

   ---------------------------
   -- In_Attribute_Fragment --
   ---------------------------

   function In_Attribute_Fragment
     (Stage : Shader_Stage;
      Name      : String;
      Type_Name : String)
      return Slice_Type
   is
   begin
      return Attribute_Fragment (Stage, Name, Type_Name,
                                 In_Variable);
   end In_Attribute_Fragment;

   ----------------------------
   -- Out_Attribute_Fragment --
   ----------------------------

   function Out_Attribute_Fragment
     (Stage : Shader_Stage;
      Name      : String;
      Type_Name : String)
      return Slice_Type
   is
   begin
      return Attribute_Fragment (Stage, Name, Type_Name,
                                 Out_Variable);
   end Out_Attribute_Fragment;

end Rho.Shaders.Slices.Attributes;
