private with WL.String_Maps;

with Rho.Shaders.Slices;

package Tau.Generators is

   type Root_Tau_Generator is abstract tagged private;

   procedure Start
     (Generator : in out Root_Tau_Generator'Class;
      Name      : String);

   procedure Set_Shader_Stage
     (Generator : in out Root_Tau_Generator;
      Stage     : Rho.Shader_Stage);

   procedure Global_Declaration
     (Generator : in out Root_Tau_Generator;
      Name      : String;
      Qualifier : Rho.Storage_Qualifier;
      Type_Name : String);

   procedure Local_Declaration
     (Generator      : in out Root_Tau_Generator;
      Name           : String;
      Type_Name      : String;
      Initialization : String);

   procedure Set_Value
     (Generator   : in out Root_Tau_Generator'Class;
      Destination : String;
      Source      : String);

   procedure Return_Value
     (Generator : in out Root_Tau_Generator'Class;
      Value     : String);

   function Shader_Function_Name
     (Generator         : Root_Tau_Generator'Class;
      Rho_Function_Name : String)
      return String;

   function Shader_Type_Name
     (Generator     : Root_Tau_Generator'Class;
      Rho_Type_Name : String)
      return String;

   function Float_Image
     (Generator : Root_Tau_Generator;
      Value     : Float)
      return String;

   function Integer_Image
     (Generator : Root_Tau_Generator;
      Value     : Integer)
      return String;

   function Normalize_Reference
     (Generator : Root_Tau_Generator;
      Name      : String)
      return String;

   function Shader_Source
     (Generator : Root_Tau_Generator;
      Stage     : Rho.Shader_Stage)
      return String;

   function Null_Generator return Root_Tau_Generator'Class;

private

   package Name_Maps is
     new WL.String_Maps (String);

   type Root_Tau_Generator is abstract tagged
      record
         Name           : Tau_String;
         Type_Names     : Name_Maps.Map;
         Function_Names : Name_Maps.Map;
         Current_Stage  : Rho.Shader_Stage;
         Slices         : Rho.Shaders.Slices.Slice_Container;
         Priority       : Rho.Shaders.Slices.Shader_Source_Priority :=
                            Rho.Shaders.Slices.Shader_Source_Priority'First;
      end record;

   function Shader_Function_Name
     (Generator         : Root_Tau_Generator'Class;
      Rho_Function_Name : String)
      return String
   is (if Generator.Function_Names.Contains (Rho_Function_Name)
       then Generator.Function_Names.Element (Rho_Function_Name)
       else Rho_Function_Name);

   function Shader_Type_Name
     (Generator     : Root_Tau_Generator'Class;
      Rho_Type_Name : String)
      return String
   is (if Generator.Type_Names.Contains (Rho_Type_Name)
       then Generator.Type_Names.Element (Rho_Type_Name)
       else Rho_Type_Name);

   function Float_Image
     (Generator : Root_Tau_Generator;
      Value     : Float)
      return String
   is (Value'Image);

   function Integer_Image
     (Generator : Root_Tau_Generator;
      Value     : Integer)
      return String
   is (Value'Image);

end Tau.Generators;
