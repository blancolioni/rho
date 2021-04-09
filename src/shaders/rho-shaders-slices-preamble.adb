package body Rho.Shaders.Slices.Preamble is

   type Root_Preamble_Type is
     new Root_Slice_Type with null record;

   overriding procedure Generate
     (Slice : Root_Preamble_Type;
      Source   : in out Source_Writer_Interface'Class);

   type Preamble_Access is access all Root_Preamble_Type'Class;

   --------------
   -- Generate --
   --------------

   overriding procedure Generate
     (Slice : Root_Preamble_Type;
      Source   : in out Source_Writer_Interface'Class)
   is
   begin
      Source.Append ("#version 330 core");
   end Generate;

   ---------------------
   -- Shader_Preamble --
   ---------------------

   function Shader_Preamble (Stage : Shader_Stage) return Slice_Type is
      Slice : constant Preamble_Access := new Root_Preamble_Type;
   begin
      Slice.Initialize
        (Name     => "preamble-" & Stage_Name (Stage),
         Stage    => Stage,
         Section  => Shader_Preamble,
         Priority => 1);
      return Slice_Type (Slice);
   end Shader_Preamble;

end Rho.Shaders.Slices.Preamble;
