private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

package Rho.Shaders.Partial is

   type Partial_Shader_Environment is interface;

   function Check
     (This : Partial_Shader_Environment;
      Name : String)
      return Boolean
      is abstract;

   type Root_Partial_Shader_Type is tagged private;

   type Partial_Shader_Type is access constant Root_Partial_Shader_Type'Class;

   function Name
     (This : Root_Partial_Shader_Type'Class)
      return String;

   function Stage
     (This : Root_Partial_Shader_Type'Class)
      return Shader_Stage;

   procedure Iterate_Lines
     (This        : Root_Partial_Shader_Type;
      Environment : Partial_Shader_Environment'Class;
      Process     : not null access
        procedure (Line : String));

   type Partial_Shader_Source is limited interface;

   procedure Iterate
     (Source : Partial_Shader_Source;
      Stage  : Shader_Stage;
      Process : not null access
        procedure (Partial : Partial_Shader_Type))
   is abstract;

   type Partial_Shader_Container is
     new Partial_Shader_Source with private;

   overriding procedure Iterate
     (Container : Partial_Shader_Container;
      Stage     : Shader_Stage;
      Process   : not null access
        procedure (Partial : Partial_Shader_Type));

   procedure Append
     (Container : in out Partial_Shader_Container;
      Partial   : Partial_Shader_Type);

private

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Root_Partial_Shader_Type is tagged
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Stage : Shader_Stage;
         Lines : String_Lists.List;
      end record;

   function Name
     (This : Root_Partial_Shader_Type'Class)
      return String
   is (Ada.Strings.Unbounded.To_String (This.Name));

   function Stage
     (This : Root_Partial_Shader_Type'Class)
      return Shader_Stage
   is (This.Stage);

   package Partial_Shader_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Partial_Shader_Type);

   type Partial_Shader_Container is
     new Partial_Shader_Source with
      record
         List : Partial_Shader_Lists.List;
      end record;

end Rho.Shaders.Partial;
