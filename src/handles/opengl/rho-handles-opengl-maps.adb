package body Rho.Handles.OpenGL.Maps is

   procedure Define_Id
     (Map  : in out Id_Maps.Map;
      Guid : WL.Guids.Guid;
      Id   : GL_Types.Uint);

   -------------------
   -- Define_Buffer --
   -------------------

   procedure Define_Buffer
     (Map    : in out Id_Map'Class;
      Buffer :    not null access Rho.Buffers.Root_Buffer_Type'Class;
      Id     :        GL_Types.Uint)
   is
   begin
      Define_Id (Map.Buffer_Map, Buffer.Guid, Id);
   end Define_Buffer;

   ---------------
   -- Define_Id --
   ---------------

   procedure Define_Id
     (Map  : in out Id_Maps.Map;
      Guid : WL.Guids.Guid;
      Id   : GL_Types.Uint)
   is
   begin
      Map.Insert (Guid, Id);
   end Define_Id;

   --------------------
   -- Define_Program --
   --------------------

   procedure Define_Program
     (Map     : in out Id_Map'Class;
      Program : not null access Rho.Shaders.Programs.Root_Program_Type'Class;
      Id      : GL_Types.Uint)
   is
   begin
      Define_Id (Map.Program_Map, Program.Guid, Id);
   end Define_Program;

   -------------------
   -- Define_Shader --
   -------------------

   procedure Define_Shader
     (Map     : in out Id_Map'Class;
      Shader  : not null access Rho.Shaders.Stages.Root_Shader_Type'Class;
      Id      : GL_Types.Uint)
   is
   begin
      Define_Id (Map.Shader_Map, Shader.Guid, Id);
   end Define_Shader;

   ---------------------
   -- Define_Variable --
   ---------------------

   procedure Define_Variable
     (Map      : in out Id_Map'Class;
      Variable : not null access
        Rho.Shaders.Variables.Root_Variable_Type'Class;
      Id       : GL_Types.Int)
   is
   begin
      Define_Id (Map.Variable_Map, Variable.Guid, GL_Types.Uint (Id));
   end Define_Variable;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
     (Map        : Id_Maps.Map;
      Object     : not null access Rho.Objects.Root_Object_Type'Class)
      return GL_Types.Uint
   is
      Guid : constant WL.Guids.Guid := Object.Guid;
   begin
      if not Map.Contains (Guid) then
         raise Constraint_Error with
           "no GL id for " & Object.Class_Name & " " & Object.Name;
      end if;
      return Map.Element (Guid);
   end Get_Id;

end Rho.Handles.OpenGL.Maps;
