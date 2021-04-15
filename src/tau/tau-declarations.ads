private with WL.String_Maps;

with Rho;

with Tau.Entries;
with Tau.Types;

with Tau.Expressions;

with Tau.Environment;
with Tau.Generators;
with Tau.Objects;

package Tau.Declarations is

   subtype Tau_Storage_Qualifier is Rho.Storage_Qualifier;

   type Root_Tau_Declaration is
     abstract new Tau.Objects.Root_Tau_Object with private;

   procedure Elaborate
     (Declaration : in out Root_Tau_Declaration;
      Environment : Tau.Environment.Tau_Environment)
   is abstract;

   procedure Compile
     (Declaration : Root_Tau_Declaration;
      Generator   : in out Tau.Generators.Root_Tau_Generator'Class)
   is abstract;

   procedure Add_Aspect
     (Declaration : in out Root_Tau_Declaration'Class;
      Name        : String;
      Value       : String);

   function Get_Type
     (Declaration : Root_Tau_Declaration'Class)
      return Tau.Types.Tau_Type;

   function Get_Entry
     (Declaration : Root_Tau_Declaration'Class)
      return Tau.Entries.Tau_Entry;

   function Get_Initializer
     (Declaration : Root_Tau_Declaration'Class)
      return Tau.Expressions.Tau_Expression;

   procedure Set_Initializer
     (Declaration : in out Root_Tau_Declaration'Class;
      Initializer : Tau.Expressions.Tau_Expression);

   function Has_Aspect
     (Declaration : Root_Tau_Declaration'Class;
      Name        : String)
      return Boolean;

   function Get_Aspect
     (Declaration : Root_Tau_Declaration'Class;
      Name        : String)
      return String;

   type Tau_Declaration is access all Root_Tau_Declaration'Class;

   function Shader_Argument_Declaration
     (Position  : GCS.Positions.File_Position;
      Name      : String;
      Type_Name : String)
      return Tau_Declaration;

   function Global_Variable_Declaration
     (Position  : GCS.Positions.File_Position;
      Name      : String;
      Qualifier : Tau_Storage_Qualifier;
      Type_Name : String)
      return Tau_Declaration;

   function Constant_Declaration
     (Position      : GCS.Positions.File_Position;
      Name          : String;
      Type_Name     : String;
      Initial_Value : Tau.Expressions.Tau_Expression)
      return Tau_Declaration;

private

   package Aspect_Maps is
     new WL.String_Maps (String);

   type Root_Tau_Declaration is
     abstract new Tau.Objects.Root_Tau_Object with
      record
         Dec_Entry : Tau.Entries.Tau_Entry;
         Dec_Type  : Tau.Types.Tau_Type;
         Aspects   : Aspect_Maps.Map;
      end record;

   overriding function Class_Name
     (Item : Root_Tau_Declaration)
      return String
   is ("declaration");

   function Get_Type
     (Declaration : Root_Tau_Declaration'Class)
      return Tau.Types.Tau_Type
   is (Declaration.Dec_Type);

   function Get_Entry
     (Declaration : Root_Tau_Declaration'Class)
      return Tau.Entries.Tau_Entry
   is (Declaration.Dec_Entry);

   function Has_Aspect
     (Declaration : Root_Tau_Declaration'Class;
      Name        : String)
      return Boolean
   is (Declaration.Aspects.Contains (Name));

   function Get_Aspect
     (Declaration : Root_Tau_Declaration'Class;
      Name        : String)
      return String
   is (Declaration.Aspects.Element (Name));

end Tau.Declarations;
