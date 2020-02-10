private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Rho.Objects;

package Rho.Shaders is

   type Root_Shader_Variable_Type is
     abstract new Rho.Objects.Root_Object_Type with private;

   function Is_Attribute
     (Variable : Root_Shader_Variable_Type)
      return Boolean;

   function Is_Uniform
     (Variable : Root_Shader_Variable_Type)
      return Boolean;

   function Element_Count
     (Variable : Root_Shader_Variable_Type)
      return Positive;

   type Shader_Variable_Type is access all Root_Shader_Variable_Type'Class;

   type Root_Shader_Type is
     new Rho.Objects.Root_Object_Type with private;

   function Stage (Shader : Root_Shader_Type) return Shader_Stage;
   function Has_Source (Shader : Root_Shader_Type) return Boolean;
   function Shader_Source (Shader : Root_Shader_Type) return String;

   procedure Set_Source (Shader : in out Root_Shader_Type;
                         Source : String);

   type Shader_Type is access all Root_Shader_Type'Class;

   function Create
     (Name   : String;
      Stage  : Shader_Stage;
      Source : String)
      return Shader_Type;

--     type Shader_Asset_Interface is limited interface;
--
--     function Shader (Shader_Assets : Shader_Asset_Interface;
--                      Stage         : Shader_Stage;
--                      Name          : String)
--                      return Shader_Type
--                      is abstract;

   type Root_Program_Type is
     new Rho.Objects.Root_Object_Type with private;

   procedure Iterate_Variables
     (Program : in out Root_Program_Type'Class;
      Process : not null access
        procedure (Variable : not null access
                       Root_Shader_Variable_Type'Class));

   function Projection_Uniform
     (Program : Root_Program_Type'Class)
      return Shader_Variable_Type;

   function Model_View_Uniform
     (Program : Root_Program_Type'Class)
      return Shader_Variable_Type;

   function Vertex_Position_Attribute
     (Program : Root_Program_Type'Class)
      return Shader_Variable_Type;

   function Vertex_Color_Attribute
     (Program : Root_Program_Type'Class)
      return Shader_Variable_Type;

   type Program_Type is access all Root_Program_Type'Class;

   function Create_Program
     (Name : String)
      return Program_Type;

   type Shader_Array is array (Positive range <>) of Shader_Type;

private

   type Root_Shader_Variable_Type is
     abstract new Rho.Objects.Root_Object_Type with
      record
         Program       : Program_Type;
         Element_Count : Positive := 1;
      end record;

   overriding function Class_Name
     (Variable : Root_Shader_Variable_Type)
      return String
   is ("shader-variable");

   function Is_Attribute
     (Variable : Root_Shader_Variable_Type)
      return Boolean
   is (False);

   function Is_Uniform
     (Variable : Root_Shader_Variable_Type)
      return Boolean
   is (False);

   function Element_Count
     (Variable : Root_Shader_Variable_Type)
      return Positive
   is (Variable.Element_Count);

   type Root_Shader_Type is
     new Rho.Objects.Root_Object_Type with
      record
         Stage  : Shader_Stage;
         Source : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Class_Name
     (Shader : Root_Shader_Type)
      return String
   is ("shader");

   package Shader_Variable_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Shader_Variable_Type);

   type Root_Program_Type is new Rho.Objects.Root_Object_Type with
      record
         Shader_Variables : Shader_Variable_Lists.List;
         Vertex_Position  : Shader_Variable_Type;
         Vertex_Color     : Shader_Variable_Type;
         Model_View       : Shader_Variable_Type;
         Projection       : Shader_Variable_Type;
      end record;

   overriding function Class_Name
     (Program : Root_Program_Type)
      return String
   is ("program");

   function Create_Attribute_Binding
     (Program       : not null access Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive)
      return Shader_Variable_Type;

   function Create_Uniform_Binding
     (Program : not null access Root_Program_Type'Class;
      Name          : String;
      Element_Count : Positive := 1)
      return Shader_Variable_Type;

   function Vertex_Position_Attribute
     (Program : Root_Program_Type'Class)
      return Shader_Variable_Type
   is (Program.Vertex_Position);

   function Vertex_Color_Attribute
     (Program : Root_Program_Type'Class)
      return Shader_Variable_Type
   is (Program.Vertex_Color);

   function Projection_Uniform
     (Program : Root_Program_Type'Class)
      return Shader_Variable_Type
   is (Program.Projection);

   function Model_View_Uniform
     (Program : Root_Program_Type'Class)
      return Shader_Variable_Type
   is (Program.Model_View);

   function Stage (Shader : Root_Shader_Type) return Shader_Stage
   is (Shader.Stage);

   function Shader_Source (Shader : Root_Shader_Type) return String
   is (Ada.Strings.Unbounded.To_String (Shader.Source));

   function Has_Source (Shader : Root_Shader_Type) return Boolean
   is (Ada.Strings.Unbounded."/="
       (Shader.Source,
        Ada.Strings.Unbounded.Null_Unbounded_String));

end Rho.Shaders;
