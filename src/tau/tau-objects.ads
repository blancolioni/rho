private with Ada.Strings.Unbounded;
private with WL.String_Maps;

with GCS.Positions;

with Rho;

package Tau.Objects is

   type Root_Tau_Object is abstract new Root_Tau_Node with private;

   type Tau_Object is access all Root_Tau_Object'Class;

   function Name
     (Object : Root_Tau_Object)
      return String;

   function Link_Name
     (Object : Root_Tau_Object)
      return String;

   procedure Initialize_Object
     (Object      : in out Root_Tau_Object'Class;
      Declaration : GCS.Positions.File_Position;
      Name        : String);

   procedure Initial_Property
     (Object        : in out Root_Tau_Object'Class;
      Name          : String;
      Initial_Value : String := "");

   function Is_Valid_Property
     (Object : Root_Tau_Object'Class;
      Name   : String)
      return Boolean;

   procedure Set_Property
     (Object : in out Root_Tau_Object'Class;
      Name   : String;
      Value  : String)
     with Pre => Object.Is_Valid_Property (Name);

   function Get_Property
     (Object : Root_Tau_Object'Class;
      Name   : String)
      return String
     with Pre => Object.Is_Valid_Property (Name);

   type Generator_Interface is interface;

   function Current_Stage
     (Generator : Generator_Interface)
      return Rho.Shader_Stage
      is abstract;

   function To_Source
     (Object    : Root_Tau_Object;
      Generator : in out Generator_Interface'Class)
      return String;

private

   package Property_Maps is
     new WL.String_Maps (String);

   type Root_Tau_Object is abstract new Root_Tau_Node with
      record
         Object_Name      : Ada.Strings.Unbounded.Unbounded_String;
         Object_Link_Name : Ada.Strings.Unbounded.Unbounded_String;
         Properties       : Property_Maps.Map;
      end record;

   function Name
     (Object : Root_Tau_Object)
      return String
   is (Ada.Strings.Unbounded.To_String (Object.Object_Name));

   function Link_Name
     (Object : Root_Tau_Object)
      return String
   is (Ada.Strings.Unbounded.To_String (Object.Object_Link_Name));

   function To_Source
     (Object    : Root_Tau_Object;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String
   is (Root_Tau_Object'Class (Object).Name);

   function Is_Valid_Property
     (Object : Root_Tau_Object'Class;
      Name   : String)
      return Boolean
   is (Object.Properties.Contains (Name));

   function Get_Property
     (Object : Root_Tau_Object'Class;
      Name   : String)
      return String
   is (Object.Properties.Element (Name));

end Tau.Objects;
