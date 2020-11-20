private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Tau.Objects;
with Tau.Types;

private package Tau.Function_Objects is

   type Root_Tau_Function_Object is
     new Tau.Objects.Root_Tau_Object with private;

   function Argument_Count
     (Fn : Root_Tau_Function_Object)
      return Natural;

   function Argument_Name
     (Fn    : Root_Tau_Function_Object;
      Index : Positive)
      return String
     with Pre'Class => Index <= Fn.Argument_Count;

   function Argument_Type
     (Fn    : Root_Tau_Function_Object;
      Index : Positive)
      return Tau.Types.Tau_Type
     with Pre'Class => Index <= Fn.Argument_Count;

   function Result_Type
     (Fn : Root_Tau_Function_Object)
      return Tau.Types.Tau_Type;

   type Tau_Function_Object is access all Root_Tau_Function_Object'Class;

private

   overriding function Class_Name
     (Item : Root_Tau_Function_Object)
      return String
   is ("function");

   type Function_Argument is
      record
         Name     : Ada.Strings.Unbounded.Unbounded_String;
         Arg_Type : Tau.Types.Tau_Type;
      end record;

   package Tau_Argument_Vectors is
     new Ada.Containers.Vectors
       (Positive, Function_Argument);

   type Root_Tau_Function_Object is
     new Tau.Objects.Root_Tau_Object with
      record
         Arguments   : Tau_Argument_Vectors.Vector;
         Result_Type : Tau.Types.Tau_Type;
      end record;

   function Argument_Count
     (Fn : Root_Tau_Function_Object)
      return Natural
   is (Fn.Arguments.Last_Index);

   function Argument_Name
     (Fn    : Root_Tau_Function_Object;
      Index : Positive)
      return String
   is (Ada.Strings.Unbounded.To_String (Fn.Arguments.Element (Index).Name));

   function Argument_Type
     (Fn    : Root_Tau_Function_Object;
      Index : Positive)
      return Tau.Types.Tau_Type
   is (Fn.Arguments.Element (Index).Arg_Type);

   function Result_Type
     (Fn : Root_Tau_Function_Object)
      return Tau.Types.Tau_Type
   is (Fn.Result_Type);

end Tau.Function_Objects;
