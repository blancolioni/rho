private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

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

   function Has_Standard_Binding
     (Program : Root_Program_Type'Class;
      Binding : Standard_Variable_Binding)
      return Boolean;

   function Standard_Binding
     (Program : Root_Program_Type'Class;
      Binding : Standard_Variable_Binding)
      return Variables.Variable_Type
     with Pre => Program.Has_Standard_Binding (Binding);
   --
   --  function Projection_Uniform
   --    (Program : Root_Program_Type'Class)
   --     return Variables.Variable_Type;
   --
   --  function Model_View_Uniform
   --    (Program : Root_Program_Type'Class)
   --     return Variables.Variable_Type;
   --
   --  function Camera_Position_Uniform
   --    (Program : Root_Program_Type'Class)
   --     return Variables.Variable_Type;
   --
   --  function Vertex_Position_Attribute
   --    (Program : Root_Program_Type'Class)
   --     return Variables.Variable_Type;
   --
   --  function Vertex_Normal_Attribute
   --    (Program : Root_Program_Type'Class)
   --     return Variables.Variable_Type;
   --
   --  function Vertex_Texture_Attribute
   --    (Program : Root_Program_Type'Class)
   --     return Variables.Variable_Type;
   --
   --  function Vertex_Color_Attribute
   --    (Program : Root_Program_Type'Class)
   --     return Variables.Variable_Type;

   function Has_Variable
     (Program : Root_Program_Type'Class;
      Name    : String)
      return Boolean;

   function Get_Variable
     (Program : Root_Program_Type'Class;
      Name    : String)
      return Variables.Variable_Type
     with Pre => Has_Variable (Program, Name);

   procedure Add_Variable
     (Program  : in out Root_Program_Type'Class;
      Variable : Variables.Variable_Type)
     with Pre => not Has_Variable (Program, Variable.Name),
     Post => Has_Variable (Program, Variable.Name);

   type Program_Type is access all Root_Program_Type'Class;

   function Create_Program
     (Name : String)
      return Program_Type;

private

   package Shader_Variable_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Variables.Variable_Type, Variables."=");

   package Shader_Variable_Maps is
     new WL.String_Maps (Rho.Shaders.Variables.Variable_Type,
                         Rho.Shaders.Variables."=");

   type Standard_Variable_Array is
     array (Standard_Variable_Binding) of Variables.Variable_Type;

   type Root_Program_Type is new Rho.Objects.Root_Object_Type with
      record
         Shader_Variables   : Shader_Variable_Lists.List;
         Variable_Map       : Shader_Variable_Maps.Map;
         Standard_Variables : Standard_Variable_Array;
      end record;

   overriding function Class_Name
     (Program : Root_Program_Type)
      return String
   is ("program");

   function Has_Standard_Binding
     (Program : Root_Program_Type'Class;
      Binding : Standard_Variable_Binding)
      return Boolean
   is (Variables."/=" (Program.Standard_Variables (Binding), null));

   function Standard_Binding
     (Program : Root_Program_Type'Class;
      Binding : Standard_Variable_Binding)
      return Variables.Variable_Type
   is (Program.Standard_Variables (Binding));

   function Has_Variable
     (Program : Root_Program_Type'Class;
      Name    : String)
      return Boolean
   is (Program.Variable_Map.Contains (Name));

   function Get_Variable
     (Program : Root_Program_Type'Class;
      Name    : String)
      return Variables.Variable_Type
   is (Program.Variable_Map.Element (Name));

end Rho.Shaders.Programs;
