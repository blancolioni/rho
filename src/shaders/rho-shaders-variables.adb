package body Rho.Shaders.Variables is

   function New_Variable
     (Program : not null access Rho.Shaders.Programs.Root_Program_Type'Class;
      Name          : String;
      Binding       : Binding_Type;
      Element_Count : Positive := 1)
      return Variable_Type;

   ---------------------------
   -- New_Attribute_Binding --
   ---------------------------

   function New_Attribute_Binding
     (Program       : not null access
        Rho.Shaders.Programs.Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive := 1)
      return Variable_Type
   is
   begin
      return New_Variable (Program, Name, Attribute_Binding, Element_Count);
   end New_Attribute_Binding;

   -------------------------
   -- New_Uniform_Binding --
   -------------------------

   function New_Uniform_Binding
     (Program       : not null access
        Rho.Shaders.Programs.Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive := 1)
      return Variable_Type
   is
   begin
      return New_Variable (Program, Name, Uniform_Binding, Element_Count);
   end New_Uniform_Binding;

   ------------------
   -- New_Variable --
   ------------------

   function New_Variable
     (Program : not null access Rho.Shaders.Programs.Root_Program_Type'Class;
      Name          : String;
      Binding       : Binding_Type;
      Element_Count : Positive := 1)
      return Variable_Type
   is
      Variable : constant Variable_Type :=
                   new Root_Variable_Type'
                     (Rho.Objects.Root_Object_Type with
                      Program       => Program_Access (Program),
                      Binding       => Binding,
                      Element_Count => Element_Count);
   begin
      Variable.Set_Name (Name);
      return Variable;
   end New_Variable;

end Rho.Shaders.Variables;
