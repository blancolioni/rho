private with Ada.Containers.Vectors;

with Rho.Objects;

package Rho.Shaders.Slices is

   type Shader_Source_Section is
     (Shader_Preamble,
      Global_Variable,
      Global_Declaration,
      Main_Function);

   type Shader_Source_Priority is range 1 .. 64;

   type Source_Writer_Interface is interface;

   procedure Append
     (Source_Writer : in out Source_Writer_Interface;
      Line          : String)
   is abstract;

   type Root_Slice_Type is
     abstract new Rho.Objects.Root_Object_Type with private;

   procedure Generate
     (Slice : Root_Slice_Type;
      Source   : in out Source_Writer_Interface'Class)
   is abstract;

   type Slice_Type is access constant Root_Slice_Type'Class;

   type Slice_Array is array (Positive range <>) of Slice_Type;

   function Stage (Slice : Root_Slice_Type'Class) return Shader_Stage;
   function Source_Section
     (Slice : Root_Slice_Type'Class)
      return Shader_Source_Section;

   function Source_Priority
     (Slice : Root_Slice_Type'Class)
      return Shader_Source_Priority;

   procedure Initialize
     (Slice    : in out Root_Slice_Type'Class;
      Name     : String;
      Stage    : Shader_Stage;
      Section  : Shader_Source_Section;
      Priority : Shader_Source_Priority);

   type Slice_Container_Interface is limited interface;

   function Shader_Slices
     (Container : Slice_Container_Interface)
      return Rho.Shaders.Slices.Slice_Array
      is abstract;

   procedure Add_Slice
     (Container : in out Slice_Container_Interface;
      Slice     : Rho.Shaders.Slices.Slice_Type)
   is abstract;

   procedure Add_Slices
     (Container : in out Slice_Container_Interface'Class;
      Slices    : Slice_Array);

   procedure Add_Slices
     (Container : in out Slice_Container_Interface'Class;
      From      : not null access constant Slice_Container_Interface'Class);

   type Slice_Container is new Slice_Container_Interface with private;

private

   type Root_Slice_Type is
     abstract new Rho.Objects.Root_Object_Type with
      record
         Stage    : Shader_Stage;
         Section  : Shader_Source_Section;
         Priority : Shader_Source_Priority;
      end record;

   overriding function Class_Name
     (Slice : Root_Slice_Type)
      return String
   is ("shader-fragment");

   function Stage
     (Slice : Root_Slice_Type'Class)
      return Shader_Stage
   is (Slice.Stage);

   function Source_Section
     (Slice : Root_Slice_Type'Class)
      return Shader_Source_Section
   is (Slice.Section);

   function Source_Priority
     (Slice : Root_Slice_Type'Class)
      return Shader_Source_Priority
   is (Slice.Priority);

   package Shader_Slice_Vectors is
     new Ada.Containers.Vectors (Positive, Slice_Type);

   type Slice_Container is new Slice_Container_Interface with
      record
         Slices    : Shader_Slice_Vectors.Vector;
      end record;

   overriding function Shader_Slices
     (Container : Slice_Container)
      return Slice_Array;

   overriding procedure Add_Slice
     (Container : in out Slice_Container;
      Slice     : Rho.Shaders.Slices.Slice_Type);

end Rho.Shaders.Slices;
