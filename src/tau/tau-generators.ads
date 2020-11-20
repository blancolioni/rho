private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Rho;

with Tau.Environment;
with Tau.Objects;

package Tau.Generators is

   type Root_Tau_Generator is
     abstract new Tau.Objects.Generator_Interface with private;

   function Get_Source
     (Generator : Root_Tau_Generator;
      Stage     : Rho.Shader_Stage)
      return String;

   function Environment
     (Generator : Root_Tau_Generator'Class)
      return Tau.Environment.Tau_Environment;

   overriding function Current_Stage
     (Generator : Root_Tau_Generator)
      return Rho.Shader_Stage;

   procedure Set_Current_Stage
     (Generator : in out Root_Tau_Generator'Class;
      Stage     : Rho.Shader_Stage);

   procedure Start_Shader
     (Generator   : in out Root_Tau_Generator;
      Name        : String;
      Stage       : Rho.Shader_Stage;
      Environment : Tau.Environment.Tau_Environment);

   procedure End_Shader
     (Generator : in out Root_Tau_Generator);

   procedure Push_Environment
     (Generator   : in out Root_Tau_Generator;
      Environment : Tau.Environment.Tau_Environment);

   procedure Pop_Environment
     (Generator   : in out Root_Tau_Generator);

   procedure Global_Declaration
     (Generator : in out Root_Tau_Generator;
      Name      : String;
      Qualifier : Rho.Storage_Qualifier;
      Type_Name : String)
   is abstract;

   procedure Local_Declaration
     (Generator      : in out Root_Tau_Generator;
      Name           : String;
      Type_Name      : String;
      Initialization : String)
   is abstract;

   function Float_Image
     (Generator : Root_Tau_Generator;
      Value     : Float)
      return String;

   function Integer_Image
     (Generator : Root_Tau_Generator;
      Value     : Integer)
      return String;

   function Shader_Type_Name
     (Generator : Root_Tau_Generator;
      Tau_Name  : String)
      return String
   is abstract;

   function Shader_Function_Name
     (Generator : Root_Tau_Generator;
      Tau_Name  : String)
      return String
   is abstract;

   procedure Set_Value
     (Generator : in out Root_Tau_Generator;
      To_Name   : String;
      Value     : String)
   is abstract;

   procedure Return_Value
     (Generator : in out Root_Tau_Generator;
      Value     : String)
   is abstract;

   procedure Add_Global_Line
     (Generator : in out Root_Tau_Generator;
      Line      : String);

   procedure Add_Main_Line
     (Generator : in out Root_Tau_Generator;
      Line      : String);

   procedure Freeze (Generator : in out Root_Tau_Generator);

private

   package Source_Line_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Environment_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Tau.Environment.Tau_Environment, Tau.Environment."=");

   type Shader_Record is
      record
         Shader_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Global_Lines : Source_Line_Lists.List;
         Main_Lines   : Source_Line_Lists.List;
         Started      : Boolean := False;
      end record;

   type Shader_Array is array (Rho.Shader_Stage) of Shader_Record;

   type Root_Tau_Generator is
     abstract new Tau.Objects.Generator_Interface with
      record
         Environment   : Tau.Environment.Tau_Environment;
         Env_Stack     : Environment_Lists.List;
         Current_Stage : Rho.Shader_Stage := Rho.Vertex_Shader;
         Shaders       : Shader_Array;
      end record;

   function Environment
     (Generator : Root_Tau_Generator'Class)
      return Tau.Environment.Tau_Environment
   is (Generator.Environment);

   overriding function Current_Stage
     (Generator : Root_Tau_Generator)
      return Rho.Shader_Stage
   is (Generator.Current_Stage);

end Tau.Generators;
