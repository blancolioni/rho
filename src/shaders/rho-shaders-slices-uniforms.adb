with Ada.Strings.Unbounded;

package body Rho.Shaders.Slices.Uniforms is

   type Uniform_Slice_Type is
     new Root_Slice_Type with
      record
         Type_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Generate
     (Slice : Uniform_Slice_Type;
      Source   : in out Source_Writer_Interface'Class);

   type Uniform_Access is access all Uniform_Slice_Type'Class;

   --------------
   -- Generate --
   --------------

   overriding procedure Generate
     (Slice : Uniform_Slice_Type;
      Source   : in out Source_Writer_Interface'Class)
   is
   begin
      Source.Append
        ("uniform"
         & " "
         & Ada.Strings.Unbounded.To_String (Slice.Type_Name)
         & " "
         & Slice.Name
         & ";");
   end Generate;

   ------------------------
   -- Uniform_Fragment --
   ------------------------

   function Uniform_Fragment
     (Stage : Shader_Stage;
      Name      : String;
      Type_Name : String)
      return Slice_Type
   is
      Uniform : constant Uniform_Access :=
                    new Uniform_Slice_Type;
   begin
      Uniform.Initialize
        (Name     => Name,
         Stage    => Stage,
         Section  => Global_Variable,
         Priority => 1);
      Uniform.Type_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Type_Name);
      return Slice_Type (Uniform);
   end Uniform_Fragment;

end Rho.Shaders.Slices.Uniforms;
