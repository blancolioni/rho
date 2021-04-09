with Ada.Strings.Unbounded;

package body Rho.Shaders.Slices.Main is

   type Root_Main_Slice_Type is
     new Root_Slice_Type with
      record
         Line : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Generate
     (Slice : Root_Main_Slice_Type;
      Source   : in out Source_Writer_Interface'Class);

   type Main_Fragment_Access is access all Root_Main_Slice_Type'Class;

   --------------
   -- Generate --
   --------------

   overriding procedure Generate
     (Slice : Root_Main_Slice_Type;
      Source   : in out Source_Writer_Interface'Class)
   is
      Base_Line : constant String :=
                    Ada.Strings.Unbounded.To_String (Slice.Line);
      Line      : constant String :=
                    (if Base_Line (Base_Line'Last) = ';'
                     then Base_Line
                     else Base_Line & ';');
   begin
      Source.Append ("  " & Line);
   end Generate;

   -----------------
   -- Shader_Line --
   -----------------

   function Shader_Line
     (Stage     : Shader_Stage;
      Priority  : Shader_Source_Priority;
      Name      : String;
      Line      : String)
      return Slice_Type
   is
      Slice : constant Main_Fragment_Access := new Root_Main_Slice_Type;
   begin
      Slice.Initialize
        (Name     => Name,
         Stage    => Stage,
         Section  => Main_Function,
         Priority => Priority);
      Slice.Line := Ada.Strings.Unbounded.To_Unbounded_String (Line);
      return Slice_Type (Slice);
   end Shader_Line;

end Rho.Shaders.Slices.Main;
