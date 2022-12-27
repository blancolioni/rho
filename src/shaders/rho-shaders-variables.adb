package body Rho.Shaders.Variables is

   function New_Variable
     (Name          : String;
      Mode          : Variable_Mode;
      Binding       : Variable_Binding_Type := No_Standard_Binding;
      Element_Count : Positive := 1)
      return Variable_Type;

   ---------------------------
   -- New_Attribute_Binding --
   ---------------------------

   function New_Attribute_Binding
     (Name          : String;
      Binding       : Variable_Binding_Type := No_Standard_Binding;
      Element_Count : Positive := 1)
      return Variable_Type
   is
   begin
      return New_Variable (Name, In_Variable, Binding, Element_Count);
   end New_Attribute_Binding;

   -------------------------
   -- New_Uniform_Binding --
   -------------------------

   function New_Uniform_Binding
     (Name          : String;
      Binding       : Variable_Binding_Type := No_Standard_Binding;
      Element_Count : Positive := 1)
      return Variable_Type
   is
   begin
      return New_Variable (Name, Uniform_Variable, Binding, Element_Count);
   end New_Uniform_Binding;

   ------------------
   -- New_Variable --
   ------------------

   function New_Variable
     (Name          : String;
      Mode          : Variable_Mode;
      Binding       : Variable_Binding_Type := No_Standard_Binding;
      Element_Count : Positive := 1)
      return Variable_Type
   is
      Variable : constant Variable_Type :=
                   new Root_Variable_Type'
                     (Rho.Objects.Root_Object_Type with
                      Mode          => Mode,
                      Binding       => Binding,
                      Element_Count => Element_Count);
   begin
      Variable.Set_Name (Name);
      return Variable;
   end New_Variable;

end Rho.Shaders.Variables;
