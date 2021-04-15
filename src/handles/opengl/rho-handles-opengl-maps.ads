private with WL.Guids.Maps;
private with Rho.Objects;

with GL_Types;

with Rho.Buffers;
with Rho.Shaders.Programs;
with Rho.Shaders.Stages;
with Rho.Shaders.Variables;

private package Rho.Handles.OpenGL.Maps is

   type Id_Map is tagged private;

   function Buffer_Id
     (Map      : Id_Map'Class;
      Buffer   : not null access Rho.Buffers.Root_Buffer_Type'Class)
      return GL_Types.Uint;

   function Program_Id
     (Map      : Id_Map'Class;
      Program  : not null access
        Rho.Shaders.Programs.Root_Program_Type'Class)
      return GL_Types.Uint;

   function Shader_Id
     (Map      : Id_Map'Class;
      Shader   : not null access
        Rho.Shaders.Stages.Root_Shader_Type'Class)
      return GL_Types.Uint;

   function Has_Id
     (Map      : Id_Map'Class;
      Variable : not null access
        Rho.Shaders.Variables.Root_Variable_Type'Class)
      return Boolean;

   function Variable_Id
     (Map      : Id_Map'Class;
      Variable : not null access
        Rho.Shaders.Variables.Root_Variable_Type'Class)
      return GL_Types.Uint;

   procedure Define_Buffer
     (Map    : in out Id_Map'Class;
      Buffer : not null access Rho.Buffers.Root_Buffer_Type'Class;
      Id     : GL_Types.Uint);

   procedure Define_Program
     (Map     : in out Id_Map'Class;
      Program : not null access Rho.Shaders.Programs.Root_Program_Type'Class;
      Id      : GL_Types.Uint);

   procedure Define_Shader
     (Map     : in out Id_Map'Class;
      Shader  : not null access Rho.Shaders.Stages.Root_Shader_Type'Class;
      Id      : GL_Types.Uint);

   procedure Define_Variable
     (Map      : in out Id_Map'Class;
      Variable : not null access
        Rho.Shaders.Variables.Root_Variable_Type'Class;
      Id       : GL_Types.Int);

private

   package Id_Maps is
     new WL.Guids.Maps (GL_Types.Uint, GL_Types."=");

   type Id_Map is tagged
      record
         Shader_Map   : Id_Maps.Map;
         Program_Map  : Id_Maps.Map;
         Variable_Map : Id_Maps.Map;
         Buffer_Map   : Id_Maps.Map;
         Argument_Map : Id_Maps.Map;
      end record;

   function Has_Id
     (Map        : Id_Maps.Map;
      Object     : not null access Rho.Objects.Root_Object_Type'Class)
      return Boolean;

   function Get_Id
     (Map        : Id_Maps.Map;
      Object     : not null access Rho.Objects.Root_Object_Type'Class)
      return GL_Types.Uint;

   function Buffer_Id
     (Map      : Id_Map'Class;
      Buffer   : not null access Rho.Buffers.Root_Buffer_Type'Class)
      return GL_Types.Uint
   is (Get_Id (Map.Buffer_Map, Buffer));

   function Program_Id
     (Map      : Id_Map'Class;
      Program  : not null access
        Rho.Shaders.Programs.Root_Program_Type'Class)
      return GL_Types.Uint
   is (Get_Id (Map.Program_Map, Program));

   function Shader_Id
     (Map      : Id_Map'Class;
      Shader   : not null access
        Rho.Shaders.Stages.Root_Shader_Type'Class)
      return GL_Types.Uint
   is (Get_Id (Map.Shader_Map, Shader));

   function Has_Id
     (Map      : Id_Map'Class;
      Variable : not null access
        Rho.Shaders.Variables.Root_Variable_Type'Class)
      return Boolean
   is (Has_Id (Map.Variable_Map, Variable));

   function Variable_Id
     (Map      : Id_Map'Class;
      Variable : not null access
        Rho.Shaders.Variables.Root_Variable_Type'Class)
      return GL_Types.Uint
   is (Get_Id (Map.Variable_Map, Variable));

end Rho.Handles.OpenGL.Maps;
