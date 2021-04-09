package body Rho.Shaders.Slices is

   ---------------
   -- Add_Slice --
   ---------------

   overriding procedure Add_Slice
     (Container : in out Slice_Container;
      Slice     : Rho.Shaders.Slices.Slice_Type)
   is
   begin
      Container.Slices.Append (Slice);
   end Add_Slice;

   ----------------
   -- Add_Slices --
   ----------------

   procedure Add_Slices
     (Container : in out Slice_Container_Interface'Class;
      Slices    : Rho.Shaders.Slices.Slice_Array)
   is
   begin
      for Slice of Slices loop
         Container.Add_Slice (Slice);
      end loop;
   end Add_Slices;

   ----------------
   -- Add_Slices --
   ----------------

   procedure Add_Slices
     (Container : in out Slice_Container_Interface'Class;
      From      : not null access constant Slice_Container_Interface'Class)
   is
   begin
      Container.Add_Slices (From.Shader_Slices);
   end Add_Slices;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Slice    : in out Root_Slice_Type'Class;
      Name     : String;
      Stage    : Shader_Stage;
      Section  : Shader_Source_Section;
      Priority : Shader_Source_Priority)
   is
   begin
      Slice.Set_Name (Name);
      Slice.Stage := Stage;
      Slice.Section := Section;
      Slice.Priority := Priority;
   end Initialize;

   -------------------
   -- Shader_Slices --
   -------------------

   overriding function Shader_Slices
     (Container : Slice_Container)
      return Rho.Shaders.Slices.Slice_Array
   is
   begin
      return Slices : Slice_Array (1 .. Container.Slices.Last_Index) do
         for I in Slices'Range loop
            Slices (I) := Container.Slices (I);
         end loop;
      end return;
   end Shader_Slices;

end Rho.Shaders.Slices;
