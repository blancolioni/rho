private with Ada.Containers.Doubly_Linked_Lists;

with Rho.Objects;

with Rho.Shaders.Variables;

package Rho.Shaders.Programs is

   type Root_Program_Type is
     new Rho.Objects.Root_Object_Type with private;

   procedure Iterate_Variables
     (Program : in out Root_Program_Type'Class;
      Process : not null access
        procedure (Variable : not null access
                     Variables.Root_Variable_Type'Class));

   function Projection_Uniform
     (Program : Root_Program_Type'Class)
      return Variables.Variable_Type;

   function Model_View_Uniform
     (Program : Root_Program_Type'Class)
      return Variables.Variable_Type;

   function Vertex_Position_Attribute
     (Program : Root_Program_Type'Class)
      return Variables.Variable_Type;

   function Vertex_Texture_Attribute
     (Program : Root_Program_Type'Class)
      return Variables.Variable_Type;

   function Vertex_Color_Attribute
     (Program : Root_Program_Type'Class)
      return Variables.Variable_Type;

   type Program_Type is access all Root_Program_Type'Class;

   function Create_Program
     (Name : String)
      return Program_Type;

private

   package Shader_Variable_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Variables.Variable_Type, Variables."=");

   type Root_Program_Type is new Rho.Objects.Root_Object_Type with
      record
         Shader_Variables : Shader_Variable_Lists.List;
         Vertex_Position  : Variables.Variable_Type;
         Vertex_Color     : Variables.Variable_Type;
         Vertex_Texture   : Variables.Variable_Type;
         Model_View       : Variables.Variable_Type;
         Projection       : Variables.Variable_Type;
      end record;

   overriding function Class_Name
     (Program : Root_Program_Type)
      return String
   is ("program");

   function Create_Attribute_Binding
     (Program       : not null access Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive)
      return Variables.Variable_Type;

   function Create_Uniform_Binding
     (Program       : not null access Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive := 1)
      return Variables.Variable_Type;

   function Vertex_Position_Attribute
     (Program : Root_Program_Type'Class)
      return Variables.Variable_Type
   is (Program.Vertex_Position);

   function Vertex_Texture_Attribute
     (Program : Root_Program_Type'Class)
      return Variables.Variable_Type
   is (Program.Vertex_Texture);

   function Vertex_Color_Attribute
     (Program : Root_Program_Type'Class)
      return Variables.Variable_Type
   is (Program.Vertex_Color);

   function Projection_Uniform
     (Program : Root_Program_Type'Class)
      return Variables.Variable_Type
   is (Program.Projection);

   function Model_View_Uniform
     (Program : Root_Program_Type'Class)
      return Variables.Variable_Type
   is (Program.Model_View);

end Rho.Shaders.Programs;
