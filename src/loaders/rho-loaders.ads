package Rho.Loaders is

   type Loader_Interface is limited interface;

   function Find_File
     (Loader    : Loader_Interface;
      Name      : String;
      Extension : String)
      return String
      is abstract;

end Rho.Loaders;
