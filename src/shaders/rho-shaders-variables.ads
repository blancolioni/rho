with Rho.Objects;

package Rho.Shaders.Variables is

   type Root_Variable_Type is
     new Rho.Objects.Root_Object_Type with private;

   type Variable_Type is access all Root_Variable_Type'Class;

   function Mode
     (Variable : Root_Variable_Type'Class)
      return Variable_Mode;

   function Binding
     (Variable : Root_Variable_Type'Class)
      return Variable_Binding_Type;

   function Element_Count
     (Variable : Root_Variable_Type'Class)
      return Positive;

   function New_Binding
     (Name          : String;
      Binding       : Standard_Variable_Binding;
      Element_Count : Positive := 1)
      return Variable_Type;

   function New_Attribute_Binding
     (Name          : String;
      Binding       : Variable_Binding_Type := No_Standard_Binding;
      Element_Count : Positive := 1)
      return Variable_Type;

   function New_Uniform_Binding
     (Name          : String;
      Binding       : Variable_Binding_Type := No_Standard_Binding;
      Element_Count : Positive := 1)
      return Variable_Type;

private

   type Root_Variable_Type is
     new Rho.Objects.Root_Object_Type with
      record
         Mode          : Variable_Mode;
         Binding       : Variable_Binding_Type;
         Element_Count : Positive := 1;
      end record;

   overriding function Class_Name
     (Variable : Root_Variable_Type)
      return String
   is ("shader-variable");

   function Element_Count
     (Variable : Root_Variable_Type'Class)
      return Positive
   is (Variable.Element_Count);

   function Binding
     (Variable : Root_Variable_Type'Class)
      return Variable_Binding_Type
   is (Variable.Binding);

   function Mode
     (Variable : Root_Variable_Type'Class)
      return Variable_Mode
   is (Variable.Mode);

end Rho.Shaders.Variables;
