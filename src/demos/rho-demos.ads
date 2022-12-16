private with Rho.Cameras;

with Rho.Handles;
with Rho.Scenes;
with Rho.Windows;

package Rho.Demos is

   type Root_Demo_Type is abstract tagged private;

   procedure Start
     (Demo   : not null access Root_Demo_Type;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type;
      Scene  : Rho.Scenes.Scene_Type);

   procedure Execute
     (Demo : not null access Root_Demo_Type;
      Handle : Rho.Handles.Handle;
      Window : Rho.Windows.Window_Type)
   is abstract;

   function Category (Demo : Root_Demo_Type) return String is abstract;
   function Name (Demo : Root_Demo_Type) return String is abstract;
   function Description (Demo : Root_Demo_Type) return String is abstract;

   type Demo_Type is access all Root_Demo_Type'Class;

   function Exists (Name : String) return Boolean;
   function Get (Name : String) return Demo_Type
     with Pre => Exists (Name);

   procedure Load_Demos;

   procedure Iterate_Demos
     (Process : not null access
        procedure (Demo : Demo_Type));

private

   type Root_Demo_Type is abstract tagged
      record
         Frame_Count : Natural := 0;
         Elapsed     : Duration := 0.0;
         Camera      : Rho.Cameras.Camera_Type;
      end record;

end Rho.Demos;
