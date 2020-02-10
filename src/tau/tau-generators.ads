private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Rho;

with Tau.Environment;

package Tau.Generators is

   type Root_Tau_Generator is abstract tagged private;

   function Get_Source
     (Generator : Root_Tau_Generator)
      return String;

   function Environment
     (Generator : Root_Tau_Generator'Class)
      return Tau.Environment.Tau_Environment;

   procedure Start_Shader
     (Generator   : in out Root_Tau_Generator;
      Name        : String;
      Stage       : Rho.Shader_Stage;
      Environment : Tau.Environment.Tau_Environment);

   procedure End_Shader
     (Generator : in out Root_Tau_Generator);

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

private

   package Source_Line_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Root_Tau_Generator is abstract tagged
      record
         Environment  : Tau.Environment.Tau_Environment;
         Shader_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Shader_Stage : Rho.Shader_Stage;
         Global_Lines : Source_Line_Lists.List;
         Main_Lines   : Source_Line_Lists.List;
      end record;

   function Environment
     (Generator : Root_Tau_Generator'Class)
      return Tau.Environment.Tau_Environment
   is (Generator.Environment);

end Tau.Generators;
