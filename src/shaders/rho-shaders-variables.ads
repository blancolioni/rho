limited with Rho.Shaders.Programs;

with Rho.Objects;

package Rho.Shaders.Variables is

   type Root_Variable_Type is
     new Rho.Objects.Root_Object_Type with private;

   type Variable_Type is access all Root_Variable_Type'Class;

   function Binding
     (Variable : Root_Variable_Type'Class)
      return Binding_Type;

   function Element_Count
     (Variable : Root_Variable_Type'Class)
      return Positive;

   function New_Attribute_Binding
     (Program       : not null access
        Rho.Shaders.Programs.Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive := 1)
      return Variable_Type;

   function New_Uniform_Binding
     (Program       : not null access
        Rho.Shaders.Programs.Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive := 1)
      return Variable_Type;

private

   type Program_Access is
     access all Rho.Shaders.Programs.Root_Program_Type'Class;

   type Root_Variable_Type is
     new Rho.Objects.Root_Object_Type with
      record
         Program       : Program_Access;
         Binding       : Binding_Type;
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
      return Binding_Type
   is (Variable.Binding);

end Rho.Shaders.Variables;
