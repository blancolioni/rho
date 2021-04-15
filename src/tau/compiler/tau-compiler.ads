private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;
private with Rho;
private with Tau.Declarations.Lists;
private with Tau.Statements.Lists;

with Tau.Generators;
with Tau.Shaders;

package Tau.Compiler is

   type Tau_Compiler is tagged private;

   procedure Add_Shader
     (Compiler : in out Tau_Compiler'Class;
      Shader   : Tau.Shaders.Tau_Shader);

   procedure Link
     (Compiler : in out Tau_Compiler'Class);

   procedure Generate
     (Compiler  : Tau_Compiler'Class;
      Generator : in out Tau.Generators.Root_Tau_Generator'Class);

private

   package Shader_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Tau.Shaders.Tau_Shader, Tau.Shaders."=");

   package Shader_Stage_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Tau.Shaders.Tau_Shader_Stage, Tau.Shaders."=");

   type Stage_List_Array is
     array (Rho.Shader_Stage) of Shader_Stage_Lists.List;

   package Declaration_Maps is
     new WL.String_Maps (Tau.Declarations.Tau_Declaration,
                         Tau.Declarations."=");

   package Dependent_Maps is
     new WL.String_Maps (Tau.Statements.Lists.List,
                         Tau.Statements.Lists."=");

   package Aggregate_Maps is
     new WL.String_Maps (Tau.Statements.Tau_Statement,
                         Tau.Statements."=");

   type Stage_Record is
      record
         In_Variables      : Tau.Declarations.Lists.List;
         Out_Variables     : Tau.Declarations.Lists.List;
         Uniform_Variables : Tau.Declarations.Lists.List;
         Local_Constants   : Tau.Declarations.Lists.List;
         Statements        : Tau.Statements.Lists.List;
         Dependencies      : Dependent_Maps.Map;
         Table             : Declaration_Maps.Map;
         Aggregate_Map     : Aggregate_Maps.Map;
      end record;

   type Stage_Record_Array is array (Rho.Shader_Stage) of Stage_Record;

   type Tau_Compiler is tagged
      record
         Shaders : Shader_Lists.List;
         Stages  : Stage_List_Array;
         Records : Stage_Record_Array;
      end record;

end Tau.Compiler;
